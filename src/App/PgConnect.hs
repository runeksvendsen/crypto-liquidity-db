-- NB: Source: 'CryptoDepth.OrderBook.Db.App.PqConnect'
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
module App.PgConnect
( pgConnectRetry
)
where


import           Protolude.Conv                     (toS)
import qualified Data.ByteString                    as BS
import qualified Control.Retry                      as RT
import qualified Database.Beam.Postgres             as Postgres
import           Control.Exception                  (IOException)
import           Data.List                          (isInfixOf)


-- | Retry failed database connection attempts a
--    maximum number of times
pgConnectRetry
    :: Word                     -- ^ Max retries
    -> BS.ByteString            -- ^ DB connection string
    -> IO Postgres.Connection
pgConnectRetry maxRetries connStr =
    RT.recovering
        (RT.constantDelay 1e6 <> RT.limitRetries (fromIntegral maxRetries))
        [RT.logRetries shouldRetry logRetry]
        (const $ Postgres.connectPostgreSQL connStr)
  where
    shouldRetry :: IOException -> IO Bool
    shouldRetry e = return $
        any (`isInfixOf` show e) retryStrings
    retryStrings =
        [ "server closed the connection unexpectedly"
        , "Connection refused"
        , "Connection timed out"
        ]
    logRetry :: Bool -> IOException -> RT.RetryStatus -> IO ()
    logRetry retrying err rs = do
        let retryStr = if retrying then "Retrying" else "Not retrying"
            attempt  = "(attempt " <> toS (show $ RT.rsIterNumber rs) <> ")"
            logFun = if retrying then putStrLn . ("WARNING: " ++)  else putStrLn . ("ERROR: " ++)
        logFun $ attempt <> " " <> retryStr <> " failure: " <> toS (show err)


-- Registrered connection error strings:

{-
libpq: failed (could not connect to server: Connection refused
    Is the server running on host "127.0.0.1" and accepting
    TCP/IP connections on port 5432?

libpq: failed (server closed the connection unexpectedly
    This probably means the server terminated abnormally
    before or while processing the request.

libpq: failed (could not send data to server: Connection timed out
-}