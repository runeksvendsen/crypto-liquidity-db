{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module App.Migrate
( migrateInteractive
)
where

import App.Monad
import Database (LiquidityDb)

import Prelude hiding ((.))
import Data.Proxy (Proxy(..))
import Database.Beam.Postgres
import Database.Beam.Schema
import qualified Database.Beam.AutoMigrate as BA


annotatedDb :: BA.AnnotatedDatabaseSettings Postgres LiquidityDb
annotatedDb = BA.defaultAnnotatedDbSettings defaultDbSettings

hsSchema :: BA.Schema
hsSchema = BA.fromAnnotatedDbSettings annotatedDb (Proxy @'[])

-- TODO: db transaction

exampleShowMigration :: AppM ()
exampleShowMigration =
    withDbConn $ \conn -> lift $ runBeamPostgresDebug putStrLn conn $ BA.printMigration $ BA.migrate conn hsSchema

exampleAutoMigration :: AppM ()
exampleAutoMigration =
    withDbConn $ \conn ->
        lift $ BA.tryRunMigrationsWithEditUpdate annotatedDb conn

migrateInteractive :: Config -> IO ()
migrateInteractive cfg = do
    putStrLn ""
    putStrLn "----------------------------------------------------"
    putStrLn "MIGRATION PLAN (if migration needed):"
    putStrLn "----------------------------------------------------"
    runAppM cfg exampleShowMigration
    putStrLn ""
    putStrLn "----------------------------------------------------"
    putStrLn "MIGRATE?"
    putStrLn "----------------------------------------------------"
    putStrLn "Would you like to run the migration on the database in the folder \"readme-db\" (will be created if it doesn't exist)? (y/n)"
    response <- getLine
    case response of
        "y" -> runAppM cfg exampleAutoMigration
        "Y" -> runAppM cfg exampleAutoMigration
        _ -> putStrLn "Exiting"
