{-# LANGUAGE NumericUnderscores #-}
module App.Main.CreateCalc
( main
, Config(..)
)
where

import Internal.Prelude
import App.Monad
import qualified App.Source as Source
import qualified App.MPQueue as PQ
import qualified App.RunCalc as RunCalc
import qualified App.Util
import App.Orphans ()
import App.Pool (withPoolPg)

import qualified Schema.Calculation as Db
import qualified Schema.RunCurrency
import qualified Schema.CalculationParameter
import qualified Query.Calculations as Calc
import qualified Query.RunCurrencies as Query
import qualified Insert.CalcParams as CP

import Data.Time.Clock (getCurrentTime, NominalDiffTime)
import qualified Control.Monad.STM as STM
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.Async as Async
import qualified Database.PostgreSQL.Simple.Notification as PgNotify
import qualified Database.PostgreSQL.Simple as PgSimple
import qualified Database.Beam as Beam
import GHC.Conc (threadDelay, forkIO)
import Data.String (IsString(fromString))

import qualified App.PgConnect
import Data.Void (Void)
import Text.Printf (printf)
import Data.List (intercalate)
import Data.Maybe (isJust)


runServices :: Has DbConn r => Config -> AppM r ()
runServices cfg = do
    a1 <- async resetUnfinishedCalculationsPoll
    a2 <- async createCalculationsPoll
    lift $ snd <$> Async.waitAny [a1, a2]
  where
    -- Set started calculations older than 'cfgMaxCalculationTime' to unstarted
    resetUnfinishedCalculationsPoll = forever $ do
        runDbTx $ Calc.resetStalledCalculations (cfgMaxCalculationTime $ cfgConstants cfg)
        lift $ threadDelay (round $ cfgDeadMonitorInterval (cfgConstants cfg) * 1e6)
    createCalculationsPoll = forever $ do
        createCalculations
        lift (threadDelay 60_000_000)

main :: Config -> IO ()
main cfg = runAppM cfg $ do
    -- Init
    runDbTx $ CP.setCalcParams (calculationParameters $ cfgParams (cfgConstants cfg))
    -- Run services
    runServices cfg
    logInfo "MAIN" "Done. Exiting..."

createCalculations :: Has DbConn r => AppM r ()
createCalculations =
    go
  where
    go = do
        rcM <- insertRunRunCurrencies
        calcLst <- insertMissingCalculations =<< lift App.Util.currentTime
        case (rcM, calcLst) of
            (Nothing, []) -> return ()
            _ -> go

insertRunRunCurrencies :: Has DbConn r => AppM r (Maybe (Db.Int32, [Text]))
insertRunRunCurrencies = do
    resM <- runDbTx Query.insertRunRunCurrencies
    case resM of
        Nothing -> return ()
        Just (runId, currencies) -> logInfo "RunCurrency" $ printf
            "RunId %d: inserted %d run currencies" runId (length currencies)
    return resM

-- insertMissingCalculations
--     :: Has DbConn r
--     => Db.UTCTime
--     -> AppM r [(Schema.RunCurrency.RunCurrency, Schema.CalculationParameter.CalcParam)]
insertMissingCalculations now = do
    calcs <- runDbTx $ Calc.insertMissingCalculations now
    unless (null calcs) $
        logInfo "Calculation" $ printf "Inserted %d calculations" (length calcs)
    return calcs
