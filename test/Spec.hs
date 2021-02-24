module Main where

import qualified Process.Spec
import qualified App.Main.Util
import           OrderBook.Graph.Internal.Prelude

import           Test.HUnit
import qualified Test.Hspec.Runner                  as Run
import           Test.Hspec                         (parallel)

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)


main :: IO ()
main = App.Main.Util.withDbPool App.Main.Util.LevelDebug $ \pool -> do
    baseUrl <- readBaseUrl >>= Process.Spec.mkClientEnv
    done <- Process.Spec.setup pool
    void $ runTestTT $ Process.Spec.tests baseUrl done

readBaseUrl :: IO String
readBaseUrl = do
    connStrM <- lookupEnv "SERVER_ADDRESS"
    let errorMsg = "Missing postgres connection string in DATABASE_URL environment variable"
    pure $ fromMaybe (error errorMsg) connStrM