{-# LANGUAGE NumDecimals #-}

module Main where

import qualified Process.Spec
import qualified Process.Prop.Graph
import qualified App.Main.Util
import           OrderBook.Graph.Internal.Prelude

import           Test.HUnit
import qualified Test.Hspec.Runner                  as Run
import           Test.Hspec                         (parallel)
import qualified Test.Hspec as Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Control.Concurrent (threadDelay)


main :: IO ()
main = App.Main.Util.withDbPool App.Main.Util.LevelDebug $ \pool -> do
    env <- readBaseUrl >>= Process.Spec.mkClientEnv
    done <- Process.Spec.setup pool
    Hspec.hspec $
        Hspec.describe "Unit tests" $
            fromHUnitTest $ Process.Spec.tests env done
    threadDelay 10e6 -- give the DB some time
    runHspec $ do
        Process.Prop.Graph.spec env done
  where
    runHspec = Run.hspecWith Run.defaultConfig

readBaseUrl :: IO String
readBaseUrl = do
    connStrM <- lookupEnv "SERVER_ADDRESS"
    let errorMsg = "Missing web-api server address in SERVER_ADDRESS environment variable"
    pure $ fromMaybe (error errorMsg) connStrM
