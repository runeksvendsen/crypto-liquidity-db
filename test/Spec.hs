module Main where

import qualified Process.Spec
import qualified App.Main.Util
import           OrderBook.Graph.Internal.Prelude

import           Test.HUnit
import qualified Test.Hspec.Runner                  as Run
import           Test.Hspec                         (parallel)


main :: IO ()
main = App.Main.Util.withDbPool App.Main.Util.LevelDebug $ \pool -> do
    void $ runTestTT $ TestList [Process.Spec.tests pool]
