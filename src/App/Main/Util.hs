{-# LANGUAGE OverloadedStrings #-}

module App.Main.Util
( withDbPool
, App.Log.LogLevel(..)
) where

import qualified App.Pool
import qualified App.Log
import qualified App.Migrate.Raw
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)


withDbPool :: App.Log.LogLevel -> (App.Pool.Pool App.Pool.Connection -> IO a) -> IO a
withDbPool logLevel f = App.Log.withStdoutLogging $ do
    App.Log.setLogTimeFormat "%T.%3q"
    App.Log.setLogLevel logLevel
    connStrM <- lookupEnv "DATABASE_URL"
    let connStr = fromMaybe (error errorMsg) connStrM
        errorMsg = "Missing postgres connection string in DATABASE_URL environment variable"
    App.Pool.withPoolPg connStr $ \pool -> do
        App.Migrate.Raw.autoMigrateIO pool
        f pool
