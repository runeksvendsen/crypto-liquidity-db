{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ApplicativeDo #-}
module App.Main.WebApi
( main
, API
, Lib.NominalDiffTime
, Run.UTCTime
, Run.Int32
, Text
, Lib.Int64
, LibCalc.Calculation
, Currency
)
where

import Universum.VarArg ((...))
import qualified App.Main.WebApi.ClientUrl as ClientUrl
import qualified App.Main.WebApi.Options as Opt
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
import qualified Schema.Path as Lib
import qualified Query.Liquidity as Lib
import qualified Query.Graph as Lib
import qualified Query.Books as Lib
import qualified Query.Calculations as Lib
import qualified Schema.Calculation as LibCalc
import qualified Query.Books

-- crypto-orderbook-db
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run

-- order-graph
import qualified OrderBook.Graph.Types.Book as G

-- beam-postgres
import qualified Database.Beam.Postgres as Pg
-- wai-cors
import qualified Network.Wai.Middleware.Cors as Cors
-- wai-extra
import qualified Network.Wai.Middleware.RequestLogger as RL


import Control.Monad.IO.Class
-- servant-server
import Servant
-- servant-client-core
import qualified Servant.Client.Free as SCF

import qualified Data.Aeson as JSON

-- warp
import qualified Network.Wai.Handler.Warp as Warp

import System.Environment (lookupEnv)
import OrderBook.Graph.Types (Currency)
import Internal.Prelude (Text, IsString (fromString), (<=<), toS)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Database.Beam.Backend (SqlSerial(SqlSerial))




main :: IO ()
main = Opt.withArgs $ \args ->
    App.Main.Util.withDbPool
        App.Main.Util.LevelDebug
        (\pool -> do
            let cfg = mkCfg pool
                port = fromIntegral $ Opt.optServerPort args
            putStrLn $ "Running on http://localhost:" ++ show port
            Warp.run port (middleware $ serve api $ mkServer cfg)
        )
  where
    api :: Proxy API
    api = Proxy
    middleware = RL.logStdoutDev . Cors.simpleCors
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
                (either throwError pure <=< (liftIO . AppLib.runAppM cfg . AppLib.runDbRaw . pgResult))
                (server timeout)


-- ### PgResult
newtype PgResult a = PgResult { pgResult :: Pg.Pg (Either ServerError a) }

pgReturn :: Pg.Pg a -> PgResult a
pgReturn = PgResult . fmap Right

instance Functor PgResult where
    fmap f (PgResult pgE) = PgResult $ fmap (fmap f) pgE
instance Applicative PgResult where
    pure a = PgResult (return $ Right a)
    PgResult pgMf <*> PgResult pgMa = PgResult $ do
        f <- pgMf
        a <- pgMa
        pure (f <*> a)
instance Monad PgResult where
    PgResult ma >>= fb = PgResult $ do
        ma >>= either (pure . Left) (pgResult . fb)
-- ### PgResult

server :: Lib.NominalDiffTime -> ServerT API PgResult
server timeout =
         pgReturn ... Lib.selectQuantities []
    :<|> pgReturn ... Lib.selectQuantities
    :<|> pgReturn ... Lib.selectAllCalculations
    :<|> pgReturn ... Lib.selectStalledCalculations timeout
    :<|> pgReturn ... Lib.selectUnfinishedCalcCount
    :<|> pgReturn ... Query.Books.runBooks
    :<|> selectNewestFinishedRunRedirect
    :<|> pgReturn ... Lib.selectNewestRunAllPaths
    :<|> pgReturn ... Lib.selectTestPathsSingle
    :<|> pgReturn ... Lib.selectNewestRunAllLiquidity
  where
    _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> newestRunSpecificPath :<|> _ :<|> _ =
        SCF.client (Proxy :: Proxy App.Main.WebApi.API)

    selectNewestFinishedRunRedirect numeraire slippage limitM = PgResult $ do
        runM <- Lib.selectNewestFinishedRunId numeraire slippage
        case runM of
            Nothing -> pure $ Left err404
            Just run -> do
                let clientF = newestRunSpecificPath run numeraire slippage limitM
                pure $ Right $ addHeader (toS $ ClientUrl.clientFUrl clientF) Nothing

type CurrencySymbolList =
    Capture' '[Description "One or more comma-separated currency symbols"] "currency_symbols" [Currency]

type API
    =    BasePath (Liquidity "all")
    :<|> BasePath (Liquidity CurrencySymbolList)
    :<|> BasePath GetAllCalcs
    :<|> BasePath GetUnfinishedCalcs
    :<|> BasePath GetUnfinishedCalcCount
    :<|> BasePath GetRunBooks
    :<|> BasePath NewestRunAllPaths
    :<|> BasePath SpecificRunAllPaths
    :<|> BasePath PathSingle
    :<|> BasePath CurrentTopLiquidity

type BasePath a = "api" :> "v1" :> a

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

type CurrentTopLiquidity =
    Summary "Get most recent liquidity for all currencies"
        :> "liquidity"
        :> "all"
        :> "newest"
        :> QueryParam "numeraire" Currency
        :> QueryParam "slippage" Double
        :> QueryParam "offset" Integer
        :> QueryParam "limit" Integer
        :> Get '[JSON] [(Run.Run, Text, Lib.Int64, Lib.Int64)]

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
        :> Get '[JSON] Lib.Int64

type GetRunBooks =
    Summary "Get run order books"
        :> "run"
        :> Capture' '[Description "Run ID (integer)"] "id" Run.RunId
        :> "books"
        :> Get '[JSON] [G.OrderBook Double]

type PathSingle =
    Summary "Get paths for single run currency"
        :> "run"
        :> Capture' '[Description "Run ID (integer)"] "id" Run.RunId
        :> "paths"
        :> Capture' '[Description "Numeraire (e.g. USD, EUR)"] "numeraire" Currency
        :> Capture' '[Description "Slippage"] "slippage" Double
        :> Capture' '[Description "Currency symbol"] "currency_symbol" Currency
        :> Get '[JSON] (Maybe Lib.TestPathsSingleRes)

type GenericRunAllPaths runIdent statusCode a =
    Summary "Get all paths for newest run"
    :> "run"
    :> runIdent
    :> "paths"
    :> Capture' '[Description "Numeraire (e.g. USD, EUR)"] "numeraire" Currency
    :> Capture' '[Description "Slippage"] "slippage" Double
    :> "all"
    :> QueryParam "limit" Word
    :> Verb 'GET statusCode '[JSON] a

type SpecificRunAllPaths =
    GenericRunAllPaths
        (Capture' '[Description "Run ID (integer)"] "run_id" Run.RunId)
        200
        (Maybe Lib.GraphData)

type NewestRunAllPaths =
    GenericRunAllPaths "newest" 302 (Headers '[Header "Location" Text] (Maybe Lib.GraphData))
