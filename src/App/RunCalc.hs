{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module App.RunCalc
( runInsertCalculation
-- , GraphCache
-- , newCache
)
where

import Internal.Prelude
import App.Monad
import qualified App.Timed
import qualified App.Log

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
import Text.Printf (printf)
import Control.Exception (try)
import Control.Exception.Base (SomeException)


{-# NOINLINE graphCache #-}
graphCache :: LRU.AtomicLRU (Db.RunId, Double) G.IBuyGraph
graphCache = unsafePerformIO $ LRU.newAtomicLRU (Just 10)


-- TODO: update:
--   * calculationStartTime
--   * calculationDurationSeconds

runInsertCalculation :: Calc.Calculation -> AppM ()
runInsertCalculation calc = do
    inputDataM <- lift $ LRU.lookup cacheKey graphCache
    inputData <- maybe
        (buildGraphAndCache <* logInfo "Process/Cache" ("Built graph. Updated cache: " ++ show cacheKey))
        (\res -> logInfo "Process/Cache" ("Cache hit for " ++ show cacheKey) >> return res)
        inputDataM
    resE <- lift $ try $ App.Timed.timeEval
        (\input -> return $ ST.runST $ G.matchOrders logger numeraire crypto input) inputData
    case resE of
        Left err -> do
            logError "Process" $ "matchOrders: " ++ show @SomeException err
            return ()
        Right ((sellPaths, buyPaths), durationSecs) -> do
            logInfo "Process" $ "Finished calculation in " ++ printf "%.2f seconds" durationSecs
            let paths = map G.pathPath sellPaths ++ map G.pathPath buyPaths
            dbRun $ Insert.PathQtys.insertAllPathQtys (Beam.pk dbCalc) paths
            logInfo "Process" $ "Inserted quantities for crypto " ++ toS crypto ++ " (" ++ toS numeraire ++ ") @ " ++ show slippage
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
    logger :: Monad m => String -> m ()
    logger = return . unsafePerformIO . App.Log.logTrace (toS $ show (Db.fromCalcId (Beam.pk dbCalc)) ++ "/Process")
    buildGraphAndCache = do
        books <- dbRun $ Books.runBooks runId -- look up order books
        let (_, buyGraph) = ST.runST $ G.buildBuyGraph logger books -- create buyGraph
        lift $ LRU.insert cacheKey buyGraph graphCache -- update cache
        return buyGraph
