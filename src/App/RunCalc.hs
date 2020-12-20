module App.RunCalc
( runInsertCalculation
-- , GraphCache
-- , newCache
)
where

import Internal.Prelude
import App.Monad
import qualified App.Timed

import qualified Schema.Calculation as Db
import qualified OrderBook.Graph as G
import qualified Query.Calculations as Calc
import qualified Query.Books as Books
import qualified Insert.PathQtys
import qualified Update.Calculation

import qualified Data.Cache.LRU.IO as LRU
import qualified Control.Monad.STM as STM
import qualified Control.Concurrent.STM.TMVar as STM
import qualified Control.Monad.ST as ST
import System.IO.Unsafe (unsafePerformIO)
import qualified Database.Beam as Beam


{-# NOINLINE graphCache #-}
graphCache :: LRU.AtomicLRU (Db.RunId, Double) G.IBuyGraph
graphCache = unsafePerformIO $ LRU.newAtomicLRU (Just 10)


-- TODO: update:
--   * calculationStartTime
--   * calculationDurationSeconds

runInsertCalculation :: Calc.Calculation -> AppM ()
runInsertCalculation calc = do
    dbRun $ Update.Calculation.updateStartTimeNow (Beam.pk dbCalc)
    inputDataM <- lift $ LRU.lookup cacheKey graphCache
    inputData <- maybe
        buildGraphAndCache
        (\res -> logInfo ("Cache hit for " ++ show cacheKey) >> return res)
        inputDataM
    ((sellPaths, buyPaths), durationSecs) <- lift $ App.Timed.timeEval
        (\input -> return $ ST.runST $ G.matchOrders noLogging numeraire crypto input) inputData
    let paths = map G.pathPath sellPaths ++ map G.pathPath buyPaths
    dbRun $ Insert.PathQtys.insertAllPathQtys (Beam.pk dbCalc) paths
    logInfo $ "Inserted quantities for crypto " ++ toS crypto ++ " (" ++ toS numeraire ++ ") @ " ++ show slippage
    dbRun $ Update.Calculation.updateDuration (Beam.pk dbCalc) (realToFrac durationSecs)
  where
    numeraire = toS $ Calc.calcNumeraire calc
    crypto = toS $ Calc.calcCrypto calc
    slippage = Db.calculationSlippage dbCalc
    runId = Db.getRunId dbCalc
    dbCalc = Calc.calcCalc calc
    cacheKey = (runId, slippage)
    noLogging :: Monad m => String -> m ()
    noLogging = const $ return ()
    buildGraphAndCache = do
        -- look up order books
        books <- dbRun $ Books.runBooks runId
        -- create buyGraph
        let (_, buyGraph) = ST.runST $ G.buildBuyGraph noLogging books
        logInfo $ "Built graph. Updated cache: " ++ show cacheKey
        -- update cache
        lift $ LRU.insert cacheKey buyGraph graphCache
        return buyGraph
