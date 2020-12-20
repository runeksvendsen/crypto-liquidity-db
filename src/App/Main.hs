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
import qualified App.Util
import App.Orphans ()
import App.Pool (withPoolPg)
import App.Migrate

import qualified Schema.Calculation as Db
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

-- TMP!
import qualified App.PgConnect
import qualified Database.PostgreSQL.Simple as PgSimple
import Control.Monad.Except (runExceptT)
import Data.Void (Void)
import Data.Time.LocalTime (utcToLocalTime)
-- TMP!


{-# NOINLINE calculationQueue #-}
calculationQueue :: PQ.MQueue Db.RunId Calc.Calculation
calculationQueue = unsafePerformIO PQ.emptyIO

runServices :: Config -> AppM Void
runServices cfg = do
    a1 <- async $ processCalculations
    a2 <- async $ monitorDeadCalculations (cfgDeadMonitorInterval cfg)
    a3 <- async $ pollingProcess
    lift $ snd <$> Async.waitAny [a1, a2, a3]

main :: Config -> String -> IO ()
main cfg connStr = runAppM cfg $ do
    -- Init
    _ <- withDbConn $ \conn -> insertOrReplaceTriggers conn
    dbRun $ CP.setCalcParams (calculationParameters cfg)
    -- Run services
    _ <- runServices cfg
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
    -- calculation <- lift $ STM.atomically $ PQ.removeMin calculationQueue
    now <- lift App.Util.currentTime
    calcM <- dbRun $ Calc.startCalculation now
    case calcM of
        Nothing ->
            lift $ threadDelay 10_000_000
        Just calculation -> do
            logInfo "Process" $ "Starting calculation " ++ show (Db.fromCalcId $ Beam.pk $ Calc.calcCalc calculation) ++ "..."
            RunCalc.runInsertCalculation calculation

pollingProcess :: AppM Void
pollingProcess = forever $ do
    now <- lift App.Util.currentTime
    handleEvent now Source.Runs
    handleEvent now Source.RunCurrencies
    handleEvent now Source.Calculations
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
    ask >>= \cfg -> lift $ forkIO $ runAppM cfg $
        case Source.parseByteString $ PgNotify.notificationChannel notification of
            Left errMsg -> logInfo "NOTIFY" $ "ERROR: " ++ errMsg
            Right source -> do
                logInfo "NOTIFY" ("Received event: " ++ show source)
                now <- lift App.Util.currentTime
                handleEvent now source

handleEvent :: Db.LocalTime -> Source.Source -> AppM ()
handleEvent _ Source.Runs =
    insertMissingRunCurrencies
handleEvent now Source.RunCurrencies = do
    dbRun $ Calc.insertMissingCalculations now
    -- logInfo "Calculation" ("Created " ++ show (length lst) ++ " new calculations")
handleEvent now Source.Calculations =
    return ()
    -- calcM <- dbRun $ Calc.startCalculation now
    -- case calcM of
    --     Nothing ->
    --         logInfo "QueueCalc" "no unprocessed calculations."
    --         -- TODO: purge IBuyGraph cache here?
    --     Just calc -> do
    --         lift $ STM.atomically $ PQ.insert (Db.getRunId $ Calc.calcCalc calc) calc calculationQueue
    --         logInfo "QueueCalc" $ "Queued calculation: " ++ show (Beam.pk $ Calc.calcCalc calc)

-- TODO
monitorDeadCalculations :: NominalDiffTime -> AppM Void
monitorDeadCalculations sleepTime = forever $ do
    -- add dead calculations to calculations-queue
    -- sleep for x seconds
    lift $ threadDelay (round $ sleepTime * 1e6)
    return ()
