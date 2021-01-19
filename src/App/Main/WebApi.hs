{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module App.Main.WebApi
( main
, API
, Lib.NominalDiffTime
, Run.UTCTime
, Run.Word32
, Text
, Lib.Word64
, LibCalc.Calculation
, Currency
)
where

import App.Main.WebApi.Orphans ()

-- crypto-liquidity-db
import qualified App.Monad as AppLib
import qualified App.Main.Util
import qualified App.Pool
import qualified App.Log
import App.Monad (Config(..), CfgConstants(..), CfgParams(..) )

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
-- wai-cors
import qualified Network.Wai.Middleware.Cors as Cors


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
main =
    App.Main.Util.withDbPool
        App.Main.Util.LevelDebug
        (\pool -> do
            let cfg = mkCfg pool
                port = 8000
            putStrLn $ "Running on http://localhost:" ++ show port
            Warp.run port (Cors.simpleCors $ serve api $ mkServer cfg)
        )
  where
    api :: Proxy API
    api = Proxy
    mkCfg pool = AppLib.Config
        { cfgConstants =
            CfgConstants
                { cfgMaxCalculationTime = 1800
                , cfgParams = CfgParams
                    { cfgNumeraires = []    -- not used
                    , cfgSlippages = []     -- not used
                    }
                , cfgDeadMonitorInterval = 0    -- not used
                }
        , cfgDbConnPool = pool
        }

mkServer
    :: AppLib.Config
    -> ServerT API Handler
mkServer cfg =
    let timeout = AppLib.cfgMaxCalculationTime $ AppLib.cfgConstants cfg in
    hoistServer (Proxy :: Proxy API)
                (liftIO . AppLib.runAppM cfg . AppLib.runDbTx . AppLib.asTx)
                (server timeout)

server :: Lib.NominalDiffTime -> ServerT API Pg.Pg
server timeout =
         Lib.selectQuantities []
    :<|> Lib.selectQuantities
    :<|> Lib.selectAllCalculations
    :<|> Lib.selectStalledCalculations timeout
    :<|> Lib.selectUnfinishedCalcCount

type CurrencySymbolList =
    Capture' '[Description "One or more comma-separated currency symbols"] "currency_symbols" [Currency]

type API
    =    Liquidity "all"
    :<|> Liquidity CurrencySymbolList
    :<|> GetAllCalcs
    :<|> GetUnfinishedCalcs
    :<|> GetUnfinishedCalcCount

type Liquidity (currencies :: k) =
    Summary "Get liquidity for one or more currencies"
        :> "liquidity"
        :> currencies
        :> QueryParam "from" Run.UTCTime
        :> QueryParam "to" Run.UTCTime
        :> QueryParam "numeraire" Currency
        :> QueryParam "slippage" Double
        :> QueryParam "limit" Word
        :> Get '[JSON] [Lib.LiquidityData]


type GetAllCalcs =
    Summary "Get unfinished calculations"
        :> "calc"
        :> "all"
        :> Get '[JSON] [LibCalc.Calculation]

type GetUnfinishedCalcs =
    Summary "Get unfinished calculations"
        :> "calc"
        :> "unfinished"
        :> "all"
        :> Get '[JSON] [LibCalc.Calculation]

type GetUnfinishedCalcCount =
    Summary "Get unfinished calculations"
        :> "calc"
        :> "unfinished"
        :> "count"
        :> Get '[JSON] Lib.Word64
