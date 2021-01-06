{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Orphans ()

-- crypto-liquidity-db
import qualified App.Monad as AppLib
import qualified App.Pool
import qualified App.Log

-- crypto-liquidity-db
import qualified Database as Lib
import qualified Schema.Currency as Lib
import qualified Query.Liquidity as Lib
import qualified Query.Books as Lib
import qualified Query.Calculations as Lib
import qualified Schema.Calculation as LibCalc

-- crypto-orderbook-db
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run

-- order-graph
import qualified OrderBook.Graph.Types.Book as G

-- beam-postgres
import qualified Database.Beam.Postgres as Pg

import Control.Monad.IO.Class
import Servant
import qualified Data.Aeson as JSON

-- warp
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (lookupEnv)
import OrderBook.Graph.Types (Currency)
import Internal.Prelude (Text)
import Data.Maybe (fromMaybe)


main :: IO ()
main = App.Log.withLogging $ do
    connStr <- dbConnStr
    App.Pool.withPoolPg connStr $ \pool -> do
        let cfg = mkCfg pool
            port = 8000
        putStrLn $ "Running on http://localhost:" ++ show port
        Warp.run port (serve api $ mkServer cfg)
  where
    api = Proxy :: Proxy API

    mkCfg pool = AppLib.Config
        { AppLib.cfgMaxCalculationTime = 600
        , AppLib.cfgDbConnPool = pool
          -- not used
        , AppLib.cfgNumeraires = []
        , AppLib.cfgSlippages = []
        , AppLib.cfgDeadMonitorInterval = 0
        }

    dbConnStr = do
        connStrM <- lookupEnv "DATABASE_URL"
        let connStr = fromMaybe (error errorMsg) connStrM
            errorMsg = "Missing postgres connection string in DATABASE_URL environment variable"
        return connStr

mkServer
    :: AppLib.Config
    -> ServerT API Handler
mkServer cfg =
    let timeout = AppLib.cfgMaxCalculationTime cfg in
    hoistServer (Proxy :: Proxy API)
                (liftIO . AppLib.runAppM cfg . AppLib.runBeamTx)
                (server timeout)

server :: Lib.NominalDiffTime -> ServerT API Pg.Pg
server timeout =
         Lib.selectQuantities
    :<|> Lib.selectUnfinishedCalculations timeout

type API
    =    Liquidity
    :<|> GetUnfinishedCalcs

type Liquidity =
    Summary "Get liquidity for one or more currencies"
        :> "liquidity"
        :> Capture' '[Description "Zero or more comma-separated currency symbols (zero = all)"] "currency_symbols" [Currency]
        :> QueryParam "from" Run.UTCTime
        :> QueryParam "to" Run.UTCTime
        :> Get '[JSON] [(Run.Word32, Text, Double, Text, Lib.Word64)]

type GetUnfinishedCalcs =
    Summary "Get unfinished calculations"
        :> "calc"
        :> "unfinished"
        :> "all"
        :> Get '[JSON] [LibCalc.Calculation]
