{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Migrate.BeamAutomigrate
( autoMigrate
, autoMigrateIO
)
where

import App.Monad
import qualified App.Log as Log
import Database

import Prelude
import Data.Proxy (Proxy(..))
import Database.Beam.Postgres
import Database.Beam.Schema
import qualified Database.Beam.AutoMigrate as BA
import qualified Database.Beam.AutoMigrate.Postgres as PgBA
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Strict as S
import Control.Monad.Except (unless, runExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Exception (throwIO)
import qualified Text.Show.Pretty as PP


autoMigrateIO :: Config -> IO ()
autoMigrateIO cfg =
    runAppM cfg autoMigrate

autoMigrate :: AppM ()
autoMigrate = do
    cfg <- R.ask
    withDbConn $ \conn -> lift $ runBeamPostgresDebug (runAppM cfg . logDebug "MIGRATE") conn $ do
        let migration = BA.migrate conn hsSchema
        isMigrated <- isMigratedM migration
        unless isMigrated $ do
            S.liftIO (printCurrentSchema conn)
            S.liftIO (runMigration conn migration)
  where
    annotatedDb :: BA.AnnotatedDatabaseSettings Postgres LiquidityDb
    annotatedDb = BA.defaultAnnotatedDbSettings defaultDbSettings
    hsSchema = BA.fromAnnotatedDbSettings annotatedDb (Proxy @'[])

runMigration :: Connection -> BA.Migration Pg -> IO ()
runMigration conn migration = do
    Log.logInfo "MIGRATE" "----------------------------------------------------"
    Log.logInfo "MIGRATE" "MIGRATION PLAN:"
    S.liftIO $ BA.printMigrationIO migration
    Log.logInfo "MIGRATE" "----------------------------------------------------"
    S.liftIO $ BA.runMigrationUnsafe conn migration

printCurrentSchema :: Connection -> IO ()
printCurrentSchema conn = do
    Log.logInfo "MIGRATE" "----------------------------------------------------"
    Log.logInfo "MIGRATE" "CURRENT SCHEMA:"
    PgBA.getSchema conn >>= PP.pPrint
    Log.logInfo "MIGRATE" "----------------------------------------------------"

isMigratedM :: BA.Migration Pg -> Pg Bool
isMigratedM m = do
    (== 0) . length <$> toEdits m

toEdits :: BA.Migration Pg -> Pg [BA.WithPriority BA.Edit]
toEdits m = do
    (a, edits) <- S.runStateT (runExceptT m) mempty
    case a of
        Left e -> S.liftIO $ throwIO e
        Right () -> return edits
