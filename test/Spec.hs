{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Util
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
import Data.List ((\\))


main :: IO ()
main = do
    env <- readBaseUrl >>= Process.Spec.mkClientEnv
    waitForApiServer env 30 1
    withSetup env $ \done -> do
        Hspec.hspec $
            Hspec.describe "Unit tests" $
                fromHUnitTest $ Process.Spec.tests env done
        runHspec $ do
            Process.Prop.Graph.spec env done
            WebApi.Spec.spec env
  where
    waitForApiServer :: SC.ClientEnv -> Int -> Int -> IO ()
    waitForApiServer env numRetries waitSeconds = do
        waitForTrue (Util.isReadyClientEnv env, "API server ready") numRetries waitSeconds

    waitForAllCalculationsDone env numRetries waitSeconds =
        waitForTrue (Util.allCalculationsProcessed env, "all calculations done") numRetries waitSeconds

    waitForTrue :: (IO Bool, String) -> Int -> Int -> IO ()
    waitForTrue (action, description) numRetries waitSeconds = do
        let go !retriesLeft = do
                isReady <- action
                when (retriesLeft <= 0) $
                    fail $ "Timed out waiting for: " <> description
                if isReady
                    then putStrLn description >> pure ()
                    else do
                        putStrLn $ "Waiting for " <> description <> ". Sleeping " <> show waitSeconds <> " seconds..."
                        threadDelay (waitSeconds * 1000000)
                        go (retriesLeft - 1)
        go numRetries

    runHspec = Run.hspecWith Run.defaultConfig

    -- Decides whether or not to run the "setup" phase (Process.Spec.setup) based on a CLI arg.
    --
    -- NOTE: HSpec is used to handle CLI args, so we peek at the given args, and then
    --       remove our custom arg before they're passed to HSpec.
    withSetup env f = do
        let noSetupArg = "--no-setup"
        args <- Arg.getArgs
        done <- if noSetupArg `elem` args
            then do
                waitForAllCalculationsDone env 20 10
                Process.Spec.unsafeManualSetup
            else App.Main.Util.withDbPool App.Main.Util.LevelDebug Process.Spec.setup
        Arg.withArgs (args \\ [noSetupArg]) $ f done

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
