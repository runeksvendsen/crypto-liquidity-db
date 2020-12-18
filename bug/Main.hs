{-# LANGUAGE NumericUnderscores #-}
module Main where

import qualified Database.PostgreSQL.Simple.Notification as PgNotify
import qualified Database.PostgreSQL.Simple as PgSimple
import Data.String (IsString(fromString))
import Control.Monad (void, forM_, forM, foldM, forever)
import Control.Exception (bracket, try)
import Control.Exception.Base (SomeException)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Data.Void (Void)
import Control.Concurrent (threadDelay, forkIO)


createTable :: PgSimple.Query
createTable = fromString $ unlines
    [ "CREATE TABLE test_table("
    , "    id SERIAL primary key"
    , ")"
    ]

createTriggerFun :: PgSimple.Query
createTriggerFun = fromString $ unlines
    [ "CREATE OR REPLACE FUNCTION do_notify()"
    , "   RETURNS trigger AS"
    , "$$"
    , "BEGIN"
    , "   PERFORM pg_notify('test_channel', NEW.id::text);"
    , "   RETURN NEW;"
    , "END;"
    , "$$ LANGUAGE plpgsql"
    ]

createTrigger :: PgSimple.Query
createTrigger = fromString $ unlines
    [ "CREATE TRIGGER test_trigger"
    , "   AFTER INSERT OR UPDATE"
    , "   ON test_table"
    , "   FOR EACH ROW"
    , "EXECUTE PROCEDURE do_notify()"
    ]

initDbQuery :: [PgSimple.Query]
initDbQuery =
    [ createTable
    , createTriggerFun
    , createTrigger
    ]

insertQuery :: PgSimple.Query
insertQuery = fromString
    "INSERT INTO test_table (id) VALUES (DEFAULT)"

main :: IO Void
main = do
    [connStr] <- getArgs
    main' (fromString connStr)

main' :: BS.ByteString -> IO Void
main' connStr = do
    -- pgConnectRetry 20 connStr >>= \conn -> forM_ initDbQuery (PgSimple.execute_ conn)
    void $ forkIO $ forever $ threadDelay 5_000_000 >> withConnection connStr (\conn -> PgSimple.execute_ conn insertQuery)
    listen connStr

listen :: BS.ByteString -> IO Void
listen connStr = forever $ do
    putStrLn "Waiting for notification..."
    notification <- pgConnectRetry 20 connStr >>= \conn ->
        PgNotify.getNotification conn <* PgSimple.close conn
    putStrLn $ "Received notification: " ++ show notification

withConnection :: BS.ByteString -> (PgSimple.Connection -> IO a) -> IO a
withConnection connStr =
    bracket
        (pgConnectRetry 20 connStr)
        PgSimple.close

-- Retry a number of times before giving up
pgConnectRetry :: Int -> BS.ByteString -> IO PgSimple.Connection
pgConnectRetry retryCount connStr =
    fromMaybe (error $ "Failed to connect to Postgres db") <$>
    foldM (\s _ -> maybe tryConnect (return . Just) s) Nothing [1..retryCount]
  where
    tryConnect = try (PgSimple.connectPostgreSQL connStr) >>= either handleErr (return . Just)
    handleErr err = do
        print (err :: SomeException)
        return Nothing
