{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Main
( main
, Config(..)
, withPoolPg
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


runServices :: Config -> AppM ()
runServices cfg = do
    a1 <- async $ processCalculationsPoll
    a2 <- async $ resetUnfinishedCalculationsPoll
    a3 <- async $ createCalculationsPoll
    lift $ snd <$> Async.waitAny [a1, a2, a3]
  where
    -- Set started calculations older than 'cfgMaxCalculationTime' to unstarted
    resetUnfinishedCalculationsPoll = forever $ do
        runBeamTx $ Calc.resetUnfinishedCalculations (cfgMaxCalculationTime cfg)
        lift $ threadDelay (round $ cfgDeadMonitorInterval cfg * 1e6)
    processCalculationsPoll = forever $
        processCalculations >> lift (threadDelay 60_000_000)
    createCalculationsPoll = forever $
        lift (threadDelay 60_000_000) >> createCalculations

main :: Config -> IO ()
main cfg = runAppM cfg $ do
    -- Init
    runBeamTx $ CP.setCalcParams (calculationParameters cfg)
    createCalculations
    -- Run services
    runServices cfg
    logInfo "MAIN" "Done. Exiting..."
  where
    insertOrReplaceTriggers conn = do
        statement <- lift $ readFile "pgsql/notify_trigger.pgsql"
        logInfo "SQL" statement
        lift $ PgSimple.withTransaction conn $
            PgSimple.executeMany conn (fromString statement) ([] :: [()])

processCalculations :: AppM ()
processCalculations = do
    now <- lift App.Util.currentTime
    calcM <- runBeamTx $ Calc.startCalculation now
    case calcM of
        Nothing -> do
            logInfo "Process" "No calculations left"
            return ()
        Just calculation -> do
            logInfo "Process" $ "Starting calculation "
                ++ show (Db.fromCalcId $ Beam.pk $ Calc.calcCalc calculation)
                ++ " (" ++ showCalc calculation ++ ")..."
            RunCalc.runInsertCalculation calculation
            processCalculations
  where
    showCalc calc =
        let dbCalc = Calc.calcCalc calc in
        intercalate "/" $
            [ show (Db.calculationRun dbCalc)
            , toS $ Calc.calcNumeraire calc
            , toS $ Calc.calcCrypto calc
            , printf "%f" (Db.calculationSlippage dbCalc)
            ]

createCalculations :: AppM ()
createCalculations =
    go
  where
    go = do
        insertRunRunCurrencies
        res <- lift App.Util.currentTime >>= insertMissingCalculations
        case res of
            [] -> return ()
            _ -> go

insertRunRunCurrencies :: AppM ()
insertRunRunCurrencies = do
    resM <- runBeamTx Query.insertRunRunCurrencies
    case resM of
        Nothing -> return ()
        Just (runId, currencies) -> logInfo "RunCurrency" $ printf
            "RunId %d: inserted %d run currencies" runId (length currencies)

insertMissingCalculations
    :: Db.LocalTime
    -> AppM [(Schema.RunCurrency.RunCurrency, Schema.CalculationParameter.CalcParam)]
insertMissingCalculations now = do
    calcParams <- runBeamTx $ Calc.insertMissingCalculations now
    unless (null calcParams) $
        logInfo "Calculation" $ printf "Inserted %d calculations" (length calcParams)
    return calcParams
