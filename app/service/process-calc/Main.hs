module Main
( main
) where

import qualified App.Main.ProcessCalc
import qualified App.Main.Util


main :: IO ()
main =
    App.Main.Util.withDbPool
        App.Main.Util.LevelDebug
        App.Main.ProcessCalc.main
