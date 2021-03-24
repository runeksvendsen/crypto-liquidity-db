{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Test

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
import qualified Data.Aeson as JSON

-- warp
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (getArgs, lookupEnv)
import Data.Maybe (isJust, fromMaybe)

-- servant-client
import qualified Servant.Client as SC

-- http-client
import qualified Network.HTTP.Client as HTTP
import Data.List (intercalate)
import Control.Exception (assert)


main :: IO ()
main = do
    [baseUrl] <- getArgs
    manager <- HTTP.newManager HTTP.defaultManagerSettings
    let clientEnv = SC.mkClientEnv manager (getBaseUrl baseUrl)
    putStrLn $ "Running on http://localhost:" ++ show port
    Warp.run port $ serve api (server clientEnv)
  where
    getBaseUrl baseUrl =
        either (error . ("Failed to parse URL: " ++) . show) id (SC.parseBaseUrl baseUrl)
    port = 8123
    api :: Proxy API
    api = Proxy


type API = Test

type Test =
    Summary "Test that everything works as expected"
        :> "test"
        :> Get '[JSON] Text

mkServer
    :: SC.ClientEnv
    -> ServerT API Handler
mkServer env =
    hoistServer (Proxy :: Proxy API)
                id
                (server env)

server :: SC.ClientEnv -> ServerT API Handler
server env = do
    testHandlerRun Test.testCaseCalc allCalculations env
    testHandlerRun Test.testCaseLiquidity allLiquidity' env
    testHandlerRun Test.testCaseBooks (runBooks (LibCalc.mkRunId 1)) env
    return "Success \\o/"
  where
    allLiquidity' = allLiquidity "USD" 0.5 Nothing Nothing Nothing

testHandlerRun
    :: Show a
    => (t -> Test.Spec a)
    -> SC.ClientM t
    -> SC.ClientEnv
    -> Handler ()
testHandlerRun testCase allCalculations' env = do
    calcLstE <- liftIO $ runClientM allCalculations'
    calcLst <- either (throw500 . ("allCalculations failed: " ++) . show) return calcLstE
    -- Run tests
    (success, output) <- liftIO $ Test.runTest (testCase calcLst)
    unless success $
        throw500 output
  where
    throw500 str = throwError $ err500 { errBody = toS str }
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
allLiquidity :<|> _ :<|> allCalculations :<|> _ :<|> _ :<|> runBooks :<|> _ :<|> _ =
    SC.client api
  where
    api :: Proxy App.Main.WebApi.API
    api = Proxy
