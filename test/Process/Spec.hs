{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Process.Spec
( tests
) where

-- crypto-liquidity-db
import Internal.Prelude
import App.Monad (lift)
import qualified App.RunCalc
import qualified Database as Db
import qualified Schema.Calculation as Schema
import qualified Schema.Currency as Schema
import qualified Schema.PathQty as Schema
import qualified Schema.Path as Schema
import qualified Insert.CalcParams as CP
import qualified Query.Calculations
import qualified App.Main.Util
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

import           Test.HUnit
import           Test.Hspec.Expectations.Pretty
import Data.String (IsString(fromString))
import Data.List (partition)
import GHC.TypeLits
import Data.Data (Proxy(Proxy))
import Data.Maybe (fromMaybe)
import Data.Bifunctor (Bifunctor(first))


tests :: App.Main.Util.Pool App.Main.Util.Connection -> Test
tests conn = TestLabel "regression" $ TestList
    [ TestLabel "test/data/double/test19.json BTC USD 0.5" $ TestList
        [ TestLabel "FULL: test/data/double/test19.json BTC USD 0.5" $ TestCase $ do
                bookList <- readBooksFile "/Users/rune/code/order-graph/test/data/double/test19.json"
                let numeraire = fromString "USD"
                testResult <- App.Monad.runAppM conn $ runCalcTest numeraire (fromString "BTC") bookList
                regressOut <- readRegressionData
                fromDbResult numeraire testResult `shouldBe` convertRegressionData regressOut
        ]
    ]

convertRegressionData
    :: ([(Double, String)], [(Double, String)])
    -> (Map Query.Int64 (Set String), Map Query.Int64 (Set String))
convertRegressionData (dataA, dataB) =
    let convert = toSetMap . map (first round)
    in (convert dataA, convert dataB)

readRegressionData :: IO ([(Double, String)], [(Double, String)])
readRegressionData =
    read <$> readFile "/Users/rune/code/order-graph/test/data/regression/double-test19.txt"

toHumanReadableLine
    :: (Schema.PathQty, Schema.Path)
    -> (Query.Int64, String)
toHumanReadableLine (pathQty, path') =
    (Schema.pathqtyQty pathQty, toS $ Query.Liquidity.prettyPathParts path')

toSetMap :: [(Query.Int64, String)] -> Map Query.Int64 (Set String)
toSetMap = MM.toMapOfSets . MM.fromList

fromDbResult
    :: Text -> [(Schema.PathQty, Schema.Path)]
    -> (Map Query.Int64 (Set String), Map Query.Int64 (Set String))
fromDbResult numeraire pathQtyPathLst =
    let (buyPaths, sellPaths) = partition ((== numeraireC) . Schema.pathStart . snd) pathQtyPathLst
        mkResult = toSetMap . map toHumanReadableLine
    in (mkResult sellPaths, mkResult buyPaths)
  where
    numeraireC = Schema.CurrencyId numeraire

readBooksFile :: FilePath -> IO [Insert.SomeOrderBook]
readBooksFile filePath = do
    map conv <$> G.readOrdersFile noLogging filePath
  where
    conv :: G.OrderBook Double -> Insert.SomeOrderBook
    conv ob =
        case someSymbolVal (toS $ G.bookVenue ob) of
            SomeSymbol (Proxy :: Proxy venue) ->
                let (base, quote) = G.baseQuote ob
                in case someSymbolVal (toS base) of
                    SomeSymbol (Proxy :: Proxy base) ->
                        case someSymbolVal (toS quote) of
                            SomeSymbol (Proxy :: Proxy quote) ->
                                Insert.SomeOrderBook (G.convertBook ob :: OB.OrderBook venue base quote)
    noLogging :: Monad m => String -> m ()
    noLogging = const $ return ()

runCalcTest
    :: App.Monad.Has App.Monad.DbConn conf
    => Text -- ^ numeraire
    -> Text -- ^ "test currency"
    -> [Insert.SomeOrderBook] -- ^ must contain at least a single order book for "test currency"
    -> App.Monad.AppM conf [(Schema.PathQty, Schema.Path)]
runCalcTest numeraire currency bookList = do
    dummyTime <- lift Clock.getCurrentTime
    _ <- App.Monad.withDbConn $ \conn -> lift $ storeBooks dummyTime conn bookList
    App.Monad.runDbTx $ CP.setCalcParams [(numeraire, 0.5)]
    _ <- App.Monad.runDbTx Query.insertRunRunCurrencies
    calcLst <- App.Monad.runDbTx $ Query.insertMissingCalculations dummyTime
    let calc = fromMaybe (error $ "Missing currency: " ++ toS currency) $
            lookup currency $ map (\calc' -> (Schema.getSymbol $ Schema.calculationCurrency calc', calc')) calcLst
    App.RunCalc.runInsertCalculation calc
    App.Monad.runDbRaw $
        Beam.runSelectReturningList $
        Beam.select $ calcResultQuery (Schema.calculationId calc)
 where
    calcResultQuery calcId = do
        calc <- Beam.all_ (Db.calculations Db.liquidityDb)
        Beam.guard_ $ Schema.calculationId calc Beam.==. Beam.val_ calcId
        pathQty <- Db.qtysForCalc calc
        path' <- Beam.all_ (Db.paths Db.liquidityDb)
        Beam.guard_ $ Schema.pathqtyPath pathQty `Beam.references_` path'
        pure (pathQty, path')

storeBooks :: Clock.UTCTime -> App.Main.Util.Connection -> [Insert.SomeOrderBook] -> IO Run.RunId
storeBooks dummyTime conn bookList = do
    (runId, _) <- Db.runDb conn $
        Insert.storeRun dummyTime dummyTime (map (dummyTime,) bookList)
    return runId
