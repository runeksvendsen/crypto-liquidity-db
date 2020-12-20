{-# LANGUAGE OverloadedStrings #-}
module App.Pool
( Data.Pool.Pool
, Data.Pool.withResource
, withPoolPg
, Connection
)
where

import qualified App.Log as Log
import qualified App.PgConnect
import qualified Data.Pool
import Database.Beam.Postgres (Connection)
import qualified Database.PostgreSQL.Simple as PgSimple
import Control.Exception (bracket)
import Internal.Prelude (toS)


createPool
    :: IO conn
    -> (conn -> IO ())
    -> IO (Data.Pool.Pool conn)
createPool openConn closeConn = do
    pool <- Data.Pool.createPool openConn closeConn 1 60 10
    -- Make this function fail immediately if 'openConn' fails
    --  (e.g. in case of invalid connection string)
    _ <- Data.Pool.withResource pool return
    Log.logInfo "Pg/Connect" "Successfully connected to database"
    return pool

withPool
    :: IO a
    -> (a -> IO ())
    -> (Data.Pool.Pool a -> IO c)
    -> IO c
withPool openConn closeConn =
    bracket
        (createPool openConn closeConn)
        Data.Pool.destroyAllResources

-- |
withPoolPg :: String -> (Data.Pool.Pool PgSimple.Connection -> IO a) -> IO a
withPoolPg connStr = do
    withPool
        (App.PgConnect.pgConnectRetry 20 (toS connStr))
        PgSimple.close
