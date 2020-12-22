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

import qualified App.PgConnect
import Data.Void (Void)
import Text.Printf (printf)
import Data.List (intercalate)


runServices :: Config -> AppM ()
runServices cfg = do
    a1 <- async $ processCalculations
    a2 <- async $ monitorDeadCalculations (cfgDeadMonitorInterval cfg)
    a3 <- async $ pollingProcess
    lift $ snd <$> Async.waitAny [a1, a2, a3]

main :: Config -> IO ()
main cfg = runAppM cfg $ do
    -- Init
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
        logInfo "RunCurrency" $ "Inserted run currencies (run_id, <currency_count>): " ++ show (map (fmap length) res)

processCalculations :: AppM ()
processCalculations = forever $ do
    now <- lift App.Util.currentTime
    calcM <- dbRun $ Calc.startCalculation now
    case calcM of
        Nothing ->
            lift $ threadDelay 10_000_000
        Just calculation -> do
            logInfo "Process" $ "Starting calculation "
                ++ show (Db.fromCalcId $ Beam.pk $ Calc.calcCalc calculation)
                ++ " (" ++ showCalc calculation ++ ")..."
            RunCalc.runInsertCalculation calculation
  where
    showCalc calc =
        let dbCalc = Calc.calcCalc calc in
        intercalate "/" $
            [ show (Db.calculationRun dbCalc)
            , toS $ Calc.calcNumeraire calc
            , toS $ Calc.calcCrypto calc
            , printf "%f" (Db.calculationSlippage dbCalc)
            ]

pollingProcess :: AppM void
pollingProcess = forever $ do
    lift App.Util.currentTime >>= handleEvent Source.Runs
    lift App.Util.currentTime >>= handleEvent Source.RunCurrencies
    lift $ threadDelay 60_000_000

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
                lift App.Util.currentTime >>= handleEvent source

handleEvent :: Source.Source -> Db.LocalTime -> AppM ()
handleEvent Source.Runs _ =
    insertMissingRunCurrencies
handleEvent Source.RunCurrencies now = do
    dbRun $ Calc.insertMissingCalculations now
    -- logInfo "Calculation" ("Created " ++ show (length lst) ++ " new calculations")
handleEvent Source.Calculations _ =
    return ()


-- TODO: implement
monitorDeadCalculations :: NominalDiffTime -> AppM void
monitorDeadCalculations sleepTime = forever $ do
    -- add dead calculations to calculations-queue
    -- sleep for x seconds
    lift $ threadDelay (round $ sleepTime * 1e6)
    return ()
