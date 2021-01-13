{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module App.Migrate.Util
( runMigrations
, Migration.Int16
, E.SomeException
)
where

import App.Monad
import Internal.Prelude (runInsertReturningOne)
import Database ( liquidityDb, LiquidityDb(migrations) )
import qualified Schema.Migration as Migration
import qualified Query.Migrations as Mig

import Database.Beam
import qualified Database.Beam.Postgres.Full as Pg
import qualified Database.Beam.Postgres as Pg
import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Backend.SQL (BeamSqlBackend,  BeamSqlBackendSyntax )
import Data.Maybe (fromMaybe)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning)
import Database.Beam.Postgres (Pg, PgCommandSyntax, Postgres)
import qualified Control.Exception as E

-- |
runMigrations
    :: forall a r. Has DbConn r
    => (Pg.Connection -> Migration.Int16 -> IO (Maybe (IO a))) -- ^ maybe migration for given fromVersion
    -> AppM r (Maybe [(Migration.Int16, a)])
        -- ^ Just Right: 'migrateFrom' executed with given @fromVersion@
        -- ^ Just Left: 'migrateFrom' threw the given exception
        -- ^ Nothing: another migration is in progress
runMigrations migrateFrom = do
    cfg <- ask
    let runDbTx' :: Pg b -> IO b
        runDbTx' = runAppM cfg . runBeamTx
    lift $ E.bracket
        (runDbTx' claimLatestMigration)
        (mapM (runDbTx' . rollback))
        (mapM (runAppM cfg . runAllMigrations))
  where
    runAllMigrations :: Has DbConn r => Migration.Int16 -> AppM r [(Migration.Int16, a)]
    runAllMigrations latestFromVersion = do
        logInfo "MIGRATE" $ "Starting migration from version " ++ show currentVersion
        runDbTx $ \conn -> lift $
            runAllMigrations' conn [] currentVersion
        where
        currentVersion = latestFromVersion + 1
        runAllMigrations' conn accum fromVersion = do
            migrationIOM <- migrateFrom conn fromVersion
            case migrationIOM of
                Nothing -> return accum
                Just migrationIO -> do
                    resA <- migrationIO
                    runBeamIO conn (Mig.addMigration fromVersion)
                    runAllMigrations' conn ((fromVersion, resA) : accum) (fromVersion + 1)

    claimLatestHandleResult lst@(_:_:_) = error $ "BUG (runMigration): got many results: " ++ show lst
    claimLatestHandleResult [] = Nothing
    claimLatestHandleResult [fromVersion] = Just fromVersion

    claimLatestMigration = fmap claimLatestHandleResult $
        Pg.runPgUpdateReturningList $
            Pg.updateReturning (migrations liquidityDb)
                (\m -> Migration.migrationInProgress m <-. val_ True)
                (\m ->
                    Migration.migrationFromVersion m ==. subquery_ selectQ
                    &&. not_ (Migration.migrationInProgress m)
                )
                Migration.migrationFromVersion

    rollback migrationFromVersion =
        runUpdate $
            update (migrations liquidityDb)
                (\m -> Migration.migrationInProgress m <-. val_ False)
                (\m ->
                    Migration.migrationFromVersion m ==. val_ migrationFromVersion
                    &&. Migration.migrationInProgress m
                )

    selectQ =
        limit_ 1 $
        orderBy_ desc_ $
        Pg.lockingFor_ Pg.PgSelectLockingStrengthUpdate Nothing $ do
            (calcLock, migration) <- Pg.locked_ (migrations liquidityDb)
            pure $ Migration.migrationFromVersion migration `Pg.withLocks_` calcLock
