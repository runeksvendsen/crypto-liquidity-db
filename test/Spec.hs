module Main where

import qualified Process.Spec
import qualified Process.Prop.Graph
import qualified WebApi.Spec
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
import qualified Servant.Client as SC


main :: IO ()
main = App.Main.Util.withDbPool App.Main.Util.LevelDebug $ \pool -> do
    env <- readBaseUrl >>= Process.Spec.mkClientEnv
    done <- Process.Spec.setup pool
    Hspec.hspec $
        Hspec.describe "Unit tests" $
            fromHUnitTest $ Process.Spec.tests env done
    runHspec $ do
        Process.Prop.Graph.spec env done
        WebApi.Spec.spec env
  where
    runHspec = Run.hspecWith Run.defaultConfig

readBaseUrl :: IO SC.BaseUrl
readBaseUrl = do
    connStrM <- lookupEnv "SERVER_ADDRESS"
    let errorMsg = "Missing web-api server address in SERVER_ADDRESS environment variable"
    baseUrlString <- maybe (fail errorMsg) pure connStrM
    let mkParseErrorMsg err = unwords
            [ "Failed to parse URL"
            , baseUrlString
            , "in SERVER_ADDRESS environment variable."
            , "Error:"
            , show err
            ]
    either (fail . mkParseErrorMsg) pure (SC.parseBaseUrl baseUrlString)
