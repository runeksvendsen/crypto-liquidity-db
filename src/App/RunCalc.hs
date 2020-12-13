module App.RunCalc
( runCalculation
-- , GraphCache
-- , newCache
)
where

import Internal.Prelude
import App.Monad

import qualified Schema.Calculation as Db
import qualified OrderBook.Graph as G
import qualified Query.Calculations as Calc
import qualified Query.Books as Books

import qualified Data.Cache.LRU.IO as LRU
import qualified Control.Monad.STM as STM
import qualified Control.Concurrent.STM.TMVar as STM
import qualified Control.Monad.ST as ST
import System.IO.Unsafe (unsafePerformIO)


{-# NOINLINE graphCache #-}
graphCache :: LRU.AtomicLRU (Db.RunId, Double) G.IBuyGraph
graphCache = unsafePerformIO $ LRU.newAtomicLRU (Just 10)


-- Db.Calculation
    -- calculationId
    -- calculationRun
    -- calculationCurrency
    -- calculationNumeraire
    -- calculationSlippage
    -- calculationStartTime
    -- calculationDurationSeconds

runCalculation :: Calc.Calculation -> AppM ([G.SellPath], [G.BuyPath])
runCalculation calc = do
    inputDataM <- lift $ LRU.lookup cacheKey graphCache
    inputData <- maybe buildGraphAndCache return inputDataM
    let result = ST.runST $
            G.matchOrders noLogging (toS $ Calc.calcNumeraire calc) (toS $ Calc.calcCrypto calc) inputData
    -- TODO: insert stuff
    return result
  where
    runId = Db.getRunId dbCalc
    dbCalc = Calc.calcCalc calc
    cacheKey = (runId, Db.calculationSlippage dbCalc)
    noLogging :: Monad m => String -> m ()
    noLogging = const $ return ()
    buildGraphAndCache = do
        -- fetch order books
        books <- dbRun $ Books.runBooks runId
        -- create buyGraph
        let (_, buyGraph) = ST.runST $ G.buildBuyGraph noLogging books
        logInfo $ "Built graph. Updated cache: " ++ show cacheKey
        -- update cache
        lift $ LRU.insert cacheKey buyGraph graphCache
        return buyGraph
