{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Process.WebApiRead
( mkClientEnv
, runPathSingleReq
, PathSingleReq(..)
, runLiquidityReq
, LiquidityReq(..)
, SC.ClientEnv
, runPathAllReq
) where

-- crypto-liquidity-db
import qualified App.RunCalc
import qualified Database as Db
import qualified Schema.Calculation as Schema
import qualified Schema.Currency as Schema
import qualified Schema.PathQty as Schema
import qualified Schema.Path as Schema
import qualified Insert.CalcParams as CP
import qualified Query.Calculations
import qualified App.Monad
import qualified Query.Liquidity
import qualified Query.Calculations as Query
import qualified Query.RunCurrencies as Query

-- crypto-orderbook-db
import qualified CryptoDepth.OrderBook.Db.Insert        as Insert
import qualified CryptoDepth.OrderBook.Db.Monad         as Db
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run

-- order-graph
import qualified OrderBook.Graph as G
import qualified OrderBook.Graph.Types.Book as G

-- orderbook
import qualified OrderBook.Types as OB

-- beam-core
import qualified Database.Beam as Beam
import Database.Beam.Backend (SqlSerial(SqlSerial))

-- time
import qualified Data.Time.Clock                        as Clock

-- multimap
import qualified Data.MultiMap as MM

-- containers
import Data.Map.Lazy (Map)
import Data.Set (Set)

import           Test.HUnit hiding (path)
import           Test.Hspec.Expectations.Pretty
import Data.String (IsString(fromString))
import Data.List (partition)
import GHC.TypeLits
import Data.Data (Proxy(Proxy))
import Data.Maybe (fromMaybe)
import Data.Bifunctor (Bifunctor(first))

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

-- servant
import Servant.API

-- servant-client
import qualified Servant.Client as SC

-- http-client
import qualified Network.HTTP.Client as HTTP
import Protolude (Bifunctor(bimap))


mkClientEnv :: String -> IO SC.ClientEnv
mkClientEnv baseUrlString = do
    manager <- HTTP.newManager HTTP.defaultManagerSettings
    let clientEnv = SC.mkClientEnv manager baseUrl
    return clientEnv
  where
    baseUrl =
        either (error . ("Failed to parse URL: " ++) . show) id (SC.parseBaseUrl baseUrlString)

data PathSingleReq = PathSingleReq LibCalc.RunId G.Currency Double G.Currency

runPathSingleReq
    :: SC.ClientEnv
    -> PathSingleReq
    -> IO (Either SC.ClientError [(Schema.PathQty, Schema.Path)])
runPathSingleReq env (PathSingleReq runId numeraire slippage currency) =
    fmap (concatMap snd . snd . handleNoResult) <$>
        SC.runClientM (pathSingle runId numeraire slippage currency) env
  where
    handleNoResult = fromMaybe (error "WebApiRead: empty result")

data LiquidityReq = LiquidityReq G.Currency Double G.Currency

runLiquidityReq
    :: SC.ClientEnv
    -> LiquidityReq
    -> IO (Either SC.ClientError [Lib.LiquidityData])
runLiquidityReq env (LiquidityReq numeraire slippage currency) =
    SC.runClientM request env
  where
    request = liquidity [currency] numeraire slippage Nothing Nothing Nothing

runPathAllReq
    :: SC.ClientEnv
    -> App.Main.WebApi.Currency
    -> Double
    -> Maybe Word
    -> IO (Either SC.ClientError (Maybe Lib.GraphData))
runPathAllReq env numeraire slippage limitM =
    SC.runClientM (discardHeaders <$> pathAll' numeraire slippage limitM) env
  where
    discardHeaders (Headers resp _) = resp

pathSingle
    :: Run.RunId
    -> App.Main.WebApi.Currency
    -> Double
    -> App.Main.WebApi.Currency
    -> SC.ClientM (Maybe Lib.TestPathsSingleRes)
liquidity
    :: [App.Main.WebApi.Currency]
    -> App.Main.WebApi.Currency
    -> Double
    -> Maybe LibCalc.UTCTime
    -> Maybe LibCalc.UTCTime
    -> Maybe Word
    -> SC.ClientM [Lib.LiquidityData]
_ :<|> liquidity :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> pathAll' :<|> _ :<|> pathSingle :<|> _ :<|> _ =
    SC.client api
  where
    api :: Proxy App.Main.WebApi.API
    api = Proxy
