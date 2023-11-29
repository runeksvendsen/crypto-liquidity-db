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
import qualified System.Environment as Arg


main :: IO ()
main = do
    args <- Arg.getArgs
    done <- if shouldPerformSetup args
        then App.Main.Util.withDbPool App.Main.Util.LevelDebug Process.Spec.setup
        else Process.Spec.unsafeManualSetup
    env <- readBaseUrl >>= Process.Spec.mkClientEnv
    Hspec.hspec $
        Hspec.describe "Unit tests" $
            fromHUnitTest $ Process.Spec.tests env done
    runHspec $ do
        Process.Prop.Graph.spec env done
        WebApi.Spec.spec env
  where
    runHspec = Run.hspecWith Run.defaultConfig

    -- Whether to run the "setup" phase (Process.Spec.setup) or not based on CLI args
    shouldPerformSetup args = case args of
        [] -> True
        ["--no-setup"] -> False
        other -> error $ "Unknown argument(s): " <> unwords other

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
