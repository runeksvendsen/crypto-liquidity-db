{-# LANGUAGE OverloadedStrings #-}
module App.Migrate.Raw where

import App.Monad
import qualified App.Log as Log
import qualified Query.Migrations as Mig
import Database
import Text.Printf (printf)
import Data.Char (isSpace)
import Control.Monad (void, forM_)
import Control.Exception (throwIO, IOException, try)
import App.Pool (Connection)
import qualified Database.PostgreSQL.Simple as PgSimple
import Data.String (IsString(fromString))
import System.IO.Error (isDoesNotExistError)



autoMigrateIO :: Config -> IO ()
autoMigrateIO cfg =
    runAppM cfg autoMigrate

autoMigrate :: AppM ()
autoMigrate =
    runDbTx go
  where
    go conn = do
        currentVersion <- runBeam conn Mig.currentVersion
        let filename = mkFilename currentVersion
        Log.logDebug "MIGRATE" $ "Attempting to read file " ++ filename
        sqlFileContentE <- lift $ try (readFile filename)
        case sqlFileContentE of
            Left ioErr ->
                if (isDoesNotExistError ioErr)
                    then Log.logInfo "MIGRATE" $ printf "Schema up-to-date. Version: %d" currentVersion
                    else lift $ throwIO ioErr
            Right queries -> do
                Log.logInfo "MIGRATE" $ printf "Migrating database from version %d" currentVersion
                lift $ executeQueries
                    (void . PgSimple.execute_ conn . fromString)
                    queries
                runBeam conn (Mig.addMigration currentVersion)
                go conn

    mkFilename currentVersion =
        let identifier :: String
            identifier = printf "migrate-cloud_db-%d" currentVersion
        in printf "pgsql/%s.pgsql" identifier

executeQueries
    :: (String -> IO ()) -- function that executes a single statement
    -> String -- containing zero or more semi colon-separated statements
    -> IO ()
executeQueries execute queries = do
    forM_ (statements queries) logAndExecute
  where
    logAndExecute string =
        Log.logDebug "SQL" string >> execute string

statements :: String -> [String]
statements str =
    filter (/= "") $ map (dropWhile isSpace) $ statements' str
  where
    statements' s =
        case dropWhile (== ';') s of
            "" -> []
            s' ->
                let (w, s'') = break (== ';') s'
                in w : statements s''
