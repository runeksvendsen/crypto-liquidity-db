{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified App.Main

main :: IO ()
main =
    let cfg = App.Main.Config ["USD"] [0.1, 0.01, 0.5] (10 * 3600) 3600
    in App.Main.main cfg
