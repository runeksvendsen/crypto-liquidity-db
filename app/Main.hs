{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified App.Main

main :: IO ()
main =
    App.Main.withPoolPg "conn strin TODO" $ \pool ->
        let cfg = App.Main.Config ["USD"] [0.1, 0.01, 0.5] (10 * 3600) 3600 pool
        in do
            App.Main.migrateInteractive cfg
            App.Main.main cfg
