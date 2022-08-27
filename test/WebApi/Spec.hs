{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module WebApi.Spec
( tests
)
where

import qualified WebApi.Test as Test

-- crypto-liquidity-db
import Internal.Prelude
import qualified App.Main.WebApi
import qualified App.Monad as AppLib
import qualified App.Main.Util
import qualified App.Pool
import qualified App.Log
import App.Monad ( Config(..), CfgConstants(..), CfgParams(..) )

-- crypto-liquidity-db
import qualified Database as Lib
import qualified Schema.Currency as Lib
import qualified Query.Liquidity as Lib
import qualified Query.Books as Lib
import qualified Query.Calculations as Lib
import qualified Schema.Calculation as LibCalc

import Control.Monad.IO.Class
import Servant

import System.Environment (getArgs, lookupEnv)
import Data.Maybe (isJust, fromMaybe)
import qualified Servant.Client as SC
import qualified Network.HTTP.Client as HTTP
import Data.List (intercalate)
import Control.Exception (assert)
import Data.Tuple (swap)

tests :: SC.ClientEnv -> IO ()
tests env = do
    _ <- testRun Test.testCaseCalc allCalculations env
    _ <- testRun Test.testCaseLiquidity allLiquidity' env
    allBooks <- testRun Test.testCaseBooks (runBooks runId) env
    let targetBook = head allBooks -- NB: The above test case fails if 'allBooks' is empty
        baseQuote = Lib.baseQuote targetBook
    _ <- testRun Test.testCaseBook (runBook' targetBook baseQuote) env
    _ <- testRun Test.testCaseBook (runBook' targetBook (swap baseQuote)) env
    pure ()
  where
    runId = LibCalc.mkRunId 1
    allLiquidity' = allLiquidity "USD" 0.5 Nothing Nothing Nothing
    runBook' targetBook (currency1, currency2) = do
        res <- runBook runId (Lib.bookVenue targetBook) (toS currency1) (toS currency2)
        pure (res, targetBook)

testRun
    :: Show a
    => (t -> Test.Spec a)
    -> SC.ClientM t
    -> SC.ClientEnv
    -> IO t
testRun testCase allCalculations' env = do
    calcLstE <- liftIO $ runClientM allCalculations'
    calcLst <- either (fail . ("testRun failed: " ++) . show) return calcLstE
    -- Run tests
    (success, output) <- liftIO $ Test.runTest (testCase calcLst)
    unless success $
        fail output
    pure calcLst
  where
    runClientM = (`SC.runClientM` env)

allCalculations :: SC.ClientM [LibCalc.Calculation]
allLiquidity
    :: App.Main.WebApi.Currency
    -> Double
    -> Maybe LibCalc.UTCTime
    -> Maybe LibCalc.UTCTime
    -> Maybe Word
    -> SC.ClientM [Lib.LiquidityData]
runBooks :: LibCalc.RunId -> SC.ClientM [Lib.OrderBook Double]
runBook
    :: LibCalc.RunId
    -> Text
    -> App.Main.WebApi.Currency
    -> App.Main.WebApi.Currency
    -> SC.ClientM (Lib.BookResult (Lib.OrderBook Double))
allLiquidity :<|> _ :<|> allCalculations :<|> _ :<|> _ :<|> runBooks :<|> runBook :<|> _ =
    SC.client api
  where
    api :: Proxy App.Main.WebApi.API
    api = Proxy
