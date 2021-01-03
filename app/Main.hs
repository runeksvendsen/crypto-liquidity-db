{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified App.Main
import qualified App.Log
import qualified App.Migrate.BeamAutomigrate
import qualified App.Migrate.Raw
import System.Environment (lookupEnv)


main :: IO ()
main = App.Log.withStdoutLogging $ do
    App.Log.setLogTimeFormat "%T.%3q"
    App.Log.setLogLevel App.Log.LevelDebug
    connStrM <- lookupEnv "DATABASE_URL"
    let connStr = maybe (error errorMsg) id connStrM
        errorMsg = "Missing postgres connection string in DATABASE_URL environment variable"
    App.Main.withPoolPg connStr $ \pool -> do
        let cfg = mkCfg pool
        App.Migrate.Raw.autoMigrateIO cfg
        App.Main.main cfg
  where
    mkCfg pool = App.Main.Config
        { App.Main.cfgNumeraires = ["USD"]
        , App.Main.cfgSlippages = [0.01, 0.1, 0.5]
        , App.Main.cfgMaxCalculationTime = 1800
        , App.Main.cfgDeadMonitorInterval = 3600
        , App.Main.cfgDbConnPool = pool
        }
