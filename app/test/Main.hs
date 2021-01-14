{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

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
    testHandler env

testHandler :: SC.ClientEnv -> Handler Text
testHandler env = do
    calcLstE <- liftIO $ runClientM allCalculations
    calcLst <- either (throw500 . ("allCalculations failed: " ++) . show) return calcLstE
    -- Assertions:
    let assertions = [not $ null calcLst, noUnfinishedCalculations calcLst]
        calcsStr = toS $ show calcLst
    if not $ all (== True) assertions
        then throw500 $ "Assertion error:\n" ++ calcsStr
        else return (toS calcsStr)
  where
    throw500 str = throwError $ err500 { errBody = toS str }
    runClientM = (`SC.runClientM` env)
    noUnfinishedCalculations calcs = all isFinishedCalculation calcs
    isFinishedCalculation calc =
        isJust (LibCalc.calculationStartTime calc)
        && isJust (LibCalc.calculationDurationSeconds calc)

allCalculations :: SC.ClientM [LibCalc.Calculation]
_ :<|> allCalculations :<|> _ =
    SC.client api
  where
    api :: Proxy App.Main.WebApi.API
    api = Proxy

instance ToHttpApiData [App.Main.WebApi.Currency] where
    toUrlPiece [] = "all"
    toUrlPiece lst = toS $ intercalate "," (map show lst)

instance JSON.FromJSON LibCalc.RunId
instance JSON.FromJSON Lib.CurrencyId
instance JSON.FromJSON LibCalc.Calculation
