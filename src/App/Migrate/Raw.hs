{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}
module App.Migrate.Raw where

import App.Monad
import qualified App.Migrate.Util as Util
import qualified App.Log as Log
import qualified Query.Migrations as Mig
import Database
import Text.Printf (printf)
import Data.Char (isSpace)
import Control.Monad (unless, void, forM_)
import Control.Exception (SomeException, throwIO, IOException, try)
import App.Pool (Connection)
import qualified Database.PostgreSQL.Simple as PgSimple
import Data.String (IsString(fromString))
import System.IO.Error (isDoesNotExistError)
import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Data.List (intercalate)


autoMigrateIO :: DbConn -> IO ()
autoMigrateIO dbc =
    runAppM dbc autoMigrate

autoMigrate
    :: Has DbConn r
    => AppM r ()
autoMigrate =
    go
  where
    go = do
        cfg <- ask
        resME <- lift $ E.try $ runAppM cfg (Util.runMigrations getMigration)
        case resME of
            Left ex -> do
                logError "MIGRATE" $ "Migration failure. Exception: " ++ show (ex :: SomeException)
                lift $ throwIO ex
            Right Nothing -> do
                let delaySeconds = 5
                logInfo "MIGRATE" $ printf
                    "Migration in progress by other service. Retrying in %d seconds..." delaySeconds
                lift (threadDelay $ delaySeconds * 1e6)
                go
            Right (Just lst) ->
                unless (null lst) $
                    logInfo "MIGRATE" $ "Migration success. fromVersions: " ++ intercalate ", " (map (show . fst) lst)

getMigration :: Connection -> Util.Int16 -> IO (Maybe (IO ()))
getMigration conn fromVersion = do
    let filename = mkFilename fromVersion
    Log.logDebug "MIGRATE" $ "Attempting to read file " ++ filename
    sqlFileContentE <- try (readFile filename)
    case sqlFileContentE of
        Left ioErr ->
            if isDoesNotExistError ioErr
                then return Nothing
                else throwIO ioErr
        Right queries -> do
            return $ Just $ do
                executeQueries
                    (void . PgSimple.execute_ conn . fromString)
                    queries
  where
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
