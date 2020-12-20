{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified App.Main
import System.Environment (getArgs)


main :: IO ()
main = do
    [connStr] <- getArgs
    App.Main.withPoolPg connStr $ \pool ->
        let cfg = App.Main.Config ["USD"] [0.1, 0.01, 0.5] (10 * 3600) 3600 pool
        in do
            App.Main.runMigration cfg
            App.Main.main cfg
