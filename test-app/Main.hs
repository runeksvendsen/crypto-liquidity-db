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
import qualified Query.Migrations as Lib
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

dbConnStr :: IO String
dbConnStr = do
    connStrM <- lookupEnv "DATABASE_URL"
    let connStr = maybe (error errorMsg) id connStrM
        errorMsg = "Missing postgres connection string in DATABASE_URL environment variable"
    return connStr

type API
    =    GetBooks
    :<|> GetUnfinishedCalcs

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
         Lib.runBooks
    :<|> Lib.selectUnfinishedCalculations timeout

type GetBooks =
    Summary "Get order books for run"
        :> "run"
        :> Capture' '[Description "Run ID"] "run_id" LibCalc.RunId
        :> "books"
        :> Get '[JSON] [Lib.OrderBook Double]

type GetUnfinishedCalcs =
    Summary "Get unfinished calculations"
        :> "calc"
        :> "unfinished"
        :> "all"
        :> Get '[JSON] [LibCalc.Calculation]
