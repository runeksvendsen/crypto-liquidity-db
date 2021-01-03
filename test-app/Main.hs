{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module Main where

import qualified App.Monad as AppLib

import qualified Database as Lib
import qualified Schema.Currency as Lib
import qualified Query.Migrations as Lib
import qualified Query.Books as Lib
import qualified Query.Calculations as Lib
import qualified Schema.Calculation as LibCalc

import Control.Monad.IO.Class
import Servant
import qualified Data.Aeson as JSON

main :: IO ()
main = undefined


type API
    =    GetBooks
    :<|> GetCalculations


mkServer timeoutTime cfg =
    hoistServer (Proxy :: Proxy API)
                (liftIO . AppLib.runAppM cfg . AppLib.runBeamTx)
                (server timeoutTime)

server timeoutTime =
         Lib.runBooks
    :<|> Lib.selectUnfinishedCalculations timeoutTime


type GetBooks =
    Summary "Get run orderbooks"
        :> "run"
        :> Capture' '[Description "Run ID"] "run_id" LibCalc.RunId
        :> "books"
        :> Get '[JSON] [Lib.OrderBook Double]

type GetCalculations =
    Summary "Get TODO"
        :> "calc"
        :> "unfinished"
        :> "all"
        :> Get '[JSON] [LibCalc.Calculation]

instance JSON.ToJSON LibCalc.Calculation
instance JSON.ToJSON Lib.CurrencyId
