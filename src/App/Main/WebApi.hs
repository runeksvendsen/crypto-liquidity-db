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
import qualified App.Main.WebApi.Cache as Cache
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

pgError :: ServerError -> PgResult a
pgError = PgResult . pure . Left

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
         selectQuantities []
    :<|> selectQuantities
    :<|> pgReturn ... Lib.selectAllCalculations
    :<|> pgReturn ... Lib.selectStalledCalculations timeout
    :<|> pgReturn ... Lib.selectUnfinishedCalcCount
    :<|> pgReturn ... Query.Books.runBooks
    :<|> fmap cacheOneMinute ... selectNewestFinishedRunRedirect
    :<|> fmap cacheTwoDays . pgReturn ... Lib.selectNewestRunAllPaths
    :<|> pgReturn ... Lib.selectTestPathsSingle
    :<|> pgReturn ... Lib.selectNewestRunAllLiquidity
    :<|> fmap cacheTwoDays ... selectQuantitiesPure
  where
    cacheTwoDays :: a -> Headers '[Header "Cache-Control" Cache.Public] a
    cacheTwoDays = addHeader $ Cache.MaxAge (2 :: Cache.Day)

    cacheOneMinute = addHeader oneMinute
    oneMinute = Cache.MaxAge (1 :: Cache.Minute)

    -- Redirect
    selectQuantities [] numeraire slippage Nothing Nothing limitM creditM = do
        run <- selectNewestFinishedRunId numeraire slippage
        let clientF = liquidityPure numeraire slippage run limitM creditM
            url = toS $ ClientUrl.clientFUrl clientF
        pgError $ err302 { errHeaders = [(fromString "Location", url), Cache.toHeader oneMinute] }

    selectQuantities currencies numeraire slippage fromM toM limitM creditM =
        let toRun = maybe (Left Lib.NewestFinishedRun) Right toM
        in pgReturn $! Lib.selectQuantities currencies numeraire slippage fromM toRun limitM creditM

    selectQuantitiesPure numeraire slippage endRunId limitM creditM =
        let toRun = Left $ Lib.SpecificRun endRunId
        in pgReturn $! Lib.selectQuantities [] numeraire slippage Nothing toRun limitM creditM

    selectNewestFinishedRunRedirect numeraire slippage limitM creditM = do
        run <- selectNewestFinishedRunId numeraire slippage
        let clientF = specificRunAllPaths run numeraire slippage limitM creditM
        pure $ addHeader (toS $ ClientUrl.clientFUrl clientF) Nothing

    selectNewestFinishedRunId numeraire slippage = PgResult $ do
        runM <- Lib.selectNewestFinishedRunId numeraire slippage
        maybe (pure $ Left err404) (pure . Right) runM

    _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> specificRunAllPaths :<|> _ :<|> _ :<|> liquidityPure =
        SCF.client (Proxy :: Proxy API)

type CurrencySymbolList =
    Capture' '[Description "One or more comma-separated currency symbols"] "currency_symbols" [Currency]

type API = BasePath API'

type API'
    =    Liquidity "all"
    :<|> Liquidity CurrencySymbolList
    :<|> GetAllCalcs
    :<|> GetUnfinishedCalcs
    :<|> GetUnfinishedCalcCount
    :<|> GetRunBooks
    :<|> NewestRunAllPaths
    :<|> SpecificRunAllPaths
    :<|> PathSingle
    :<|> CurrentTopLiquidity
    :<|> LiquidityPure

type BasePath a = "api" :> "v1" :> a

type IncludeCreditInstruments = QueryParam "include_credit_instruments" Bool

type Liquidity (currencies :: k) =
    Summary "Get liquidity for one or more currencies"
        :> "liquidity"
        :> currencies
        :> Capture' '[Description "Numeraire (e.g. USD, EUR)"] "numeraire" Currency
        :> Capture' '[Description "Slippage"] "slippage" Double
        :> QueryParam "from" Run.UTCTime
        :> QueryParam "to" Run.UTCTime
        :> QueryParam "limit" Word
        :> IncludeCreditInstruments
        :> Get '[JSON] [Lib.LiquidityData]

-- | Liquidity from the first run to a specific run.
--   Given the same arguments will always return the same data (hence cacheable with infinite TTL).
type LiquidityPure =
    Summary "Get liquidity for one or more currencies"
        :> "liquidity"
        :> "all"
        :> Capture' '[Description "Numeraire (e.g. USD, EUR)"] "numeraire" Currency
        :> Capture' '[Description "Slippage"] "slippage" Double
        :> Capture' '[Description "End run"] "run_id" Run.RunId
        :> QueryParam "limit" Word
        :> IncludeCreditInstruments
        :> Get '[JSON] (Headers '[Header "Cache-Control" Cache.Public] [Lib.LiquidityData])

type CurrentTopLiquidity =
    Summary "Get most recent liquidity for all currencies"
        :> "liquidity"
        :> "all"
        :> "newest"
        :> Capture' '[Description "Numeraire (e.g. USD, EUR)"] "numeraire" Currency
        :> Capture' '[Description "Slippage"] "slippage" Double
        :> QueryParam "offset" Integer
        :> QueryParam "limit" Integer
        :> IncludeCreditInstruments
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
    :> IncludeCreditInstruments
    :> Verb 'GET statusCode '[JSON] a

type SpecificRunAllPaths =
    GenericRunAllPaths
        (Capture' '[Description "Run ID (integer)"] "run_id" Run.RunId)
        200
        (Headers '[Header "Cache-Control" Cache.Public] (Maybe Lib.GraphData))

type NewestRunAllPaths =
    GenericRunAllPaths
        "newest"
        302
        (Headers '[Header "Cache-Control" Cache.Public, Header "Location" Text] (Maybe Lib.GraphData))
