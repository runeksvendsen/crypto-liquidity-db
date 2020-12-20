{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Main
( main
, Config(..)
, withPoolPg
, autoMigrate
, autoMigrateIO
)
where

import Internal.Prelude
import App.Monad
import qualified App.Source as Source
import qualified App.MPQueue as PQ
import qualified App.RunCalc as RunCalc
import App.Orphans ()
import App.Pool (withPoolPg)
import App.Migrate

import qualified Schema.Calculation as Db
import qualified Query.Calculations as Calc
import qualified Query.RunCurrencies as Query
import qualified Insert.CalcParams as CP

import Data.Time.Clock (NominalDiffTime)
import qualified Control.Monad.STM as STM
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.Async as Async
import qualified Database.PostgreSQL.Simple.Notification as PgNotify
import qualified Database.PostgreSQL.Simple as PgSimple
import qualified Database.Beam as Beam
import qualified Control.Monad.Reader as R
import GHC.Conc (threadDelay, forkIO)
import Data.String (IsString(fromString))

-- TMP!
import qualified App.PgConnect
import qualified Database.PostgreSQL.Simple as PgSimple
import Control.Monad.Except (runExceptT)
import Data.Void (Void)
-- TMP!


{-# NOINLINE calculationQueue #-}
calculationQueue :: PQ.MQueue Db.RunId Calc.Calculation
calculationQueue = unsafePerformIO PQ.emptyIO

main :: Config -> String -> IO ()
main cfg connStr = runAppM cfg $ do
    -- Init
    _ <- withDbConn $ \conn -> insertOrReplaceTriggers conn
    dbRun $ CP.setCalcParams (calculationParameters cfg)
    -- Go
    a1 <- async $ processCalculations
    a2 <- async $ monitorDeadCalculations (cfgDeadMonitorInterval cfg)
    a3 <- async $ pollingProcess
    insertMissingRunCurrencies
    _ <- lift $ snd <$> Async.waitAny [a1, a2, a3]
    logInfo "MAIN" "Done. Exiting..."
  where
    insertOrReplaceTriggers conn = do
        statement <- lift $ readFile "pgsql/notify_trigger.pgsql"
        logInfo "SQL" statement
        lift $ PgSimple.withTransaction conn $
            PgSimple.executeMany conn (fromString statement) ([] :: [()])

insertMissingRunCurrencies :: AppM ()
insertMissingRunCurrencies = do
    res <- dbRun Query.insertMissingRunCurrencies
    unless (null res) $
        logInfo "RunCurrency" $ "Inserted run currencies: " ++ show res

processCalculations :: AppM Void
processCalculations = forever $ do
    calculation <- lift $ STM.atomically $ PQ.removeMin calculationQueue
    RunCalc.runInsertCalculation calculation

pollingProcess :: AppM Void
pollingProcess = forever $ do
    handleEvent Source.Runs
    handleEvent Source.RunCurrencies
    handleEvent Source.Calculations
    lift $ threadDelay 10_000_000

-- | Listen for:
--    * INSERT runs                 ->  insertMissingRunCurrencies
--    * INSERT run_currencies       ->  insertMissingCalculations
--    * INSERT calculations         ->  add calculations to calculations-queue
notificationsListen :: String -> AppM Void
notificationsListen connStr = forever $ do
    logInfo "NOTIFY" "Waiting for notification..."
    -- TMP
    conn <- lift $ App.PgConnect.pgConnectRetry 20 (toS connStr)
    notification <- lift $ PgNotify.getNotification conn
    lift $ PgSimple.close conn
    -- TMP
    logDebug "NOTIFY" $ "Received notification. Raw: " ++ show notification
    R.ask >>= \cfg -> lift $ forkIO $ runAppM cfg $
        case Source.parseByteString $ PgNotify.notificationChannel notification of
            Left errMsg -> logInfo "NOTIFY" $ "ERROR: " ++ errMsg
            Right source -> logInfo "NOTIFY" ("Received event: " ++ show source) >> handleEvent source

handleEvent :: Source.Source -> AppM ()
handleEvent Source.Runs =
    insertMissingRunCurrencies
handleEvent Source.RunCurrencies = do
    lst <- dbRun Calc.insertMissingCalculations
    logInfo "Calculation" ("Created " ++ show (length lst) ++ " new calculations")
handleEvent Source.Calculations = do
    calcM <- dbRun Calc.startCalculation
    case calcM of
        Nothing ->
            logInfo "Calculation" "'startCalculation' returned Nothing: no unprocessed calculations."
            -- TODO: purge IBuyGraph cache here?
        Just calc -> do
            R.lift $ STM.atomically $ PQ.insert (Db.getRunId $ Calc.calcCalc calc) calc calculationQueue
            logInfo "Calculation" $ "Inserted calculation: " ++ show (Beam.pk $ Calc.calcCalc calc)

-- TODO
monitorDeadCalculations :: NominalDiffTime -> AppM Void
monitorDeadCalculations sleepTime = forever $ do
    -- add dead calculations to calculations-queue
    -- sleep for x seconds
    lift $ threadDelay (round $ sleepTime * 1e6)
    return ()
