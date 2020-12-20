{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified App.Main
import qualified App.Log
import System.Environment (lookupEnv)


main :: IO ()
main = App.Log.withStdoutLogging $ do
    connStrM <- lookupEnv "DATABASE_URL"
    let connStr = maybe (error errorMsg) id connStrM
        errorMsg = "Missing postgres connection string in DATABASE_URL environment variable"
    App.Main.withPoolPg connStr $ \pool ->
        let cfg = App.Main.Config ["USD"] [0.01, 0.1, 0.5] (10 * 3600) 3600 pool
        in do
            App.Main.autoMigrateIO cfg
            App.Main.main cfg connStr
