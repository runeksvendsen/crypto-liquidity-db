{-# LANGUAGE OverloadedStrings #-}
module App.Main
( main
, Config(..)
, withPoolPg
, migrateInteractive
)
where

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

import Data.Time.Clock (NominalDiffTime)
import qualified Control.Monad.STM as STM
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.Async as Async
import qualified Database.PostgreSQL.Simple.Notification as PgNotify
import qualified Database.Beam as Beam
import qualified Control.Monad.Reader as R
import GHC.Conc (threadDelay)


{-# NOINLINE calculationQueue #-}
calculationQueue :: PQ.MQueue Db.RunId Calc.Calculation
calculationQueue = unsafePerformIO PQ.emptyIO

main :: Config -> IO ()
main cfg = do
    a1 <- Async.async $ runAppM cfg processCalculations
    a2 <- Async.async $ monitorDeadCalculations (cfgDeadMonitorInterval cfg)
    a3 <- Async.async $ runAppM cfg notificationsListen
    runAppM cfg insertMissingRunCurrencies
    snd <$> Async.waitAny [a1, a2, a3]

insertMissingRunCurrencies :: AppM ()
insertMissingRunCurrencies = do
    res <- dbRun Query.insertMissingRunCurrencies
    logInfo $ "Inserted run currencies: " ++ show res

processCalculations :: AppM ()
processCalculations = do
    calculation <- lift $ STM.atomically $ PQ.removeMin calculationQueue
    RunCalc.runInsertCalculation calculation

notificationsListen :: AppM ()
notificationsListen = do
    -- listen:
    --    * INSERT runs                 ->  insertMissingRunCurrencies
    --    * INSERT run_currencies       ->  insertMissingCalculations
    --    * INSERT/UPDATE calculations  ->  add calculations to calculations-queue
    notification <- withDbConn (R.lift . PgNotify.getNotification)
    case Source.parseByteString $ PgNotify.notificationChannel notification of
        Left errMsg -> logInfo $ "ERROR: "++ errMsg
        Right source -> logInfo ("Received event: " ++ show source) >> handleEvent source
  where
    handleEvent Source.Runs =
        insertMissingRunCurrencies
    handleEvent Source.RunCurrencies = do
            lst <- dbRun Calc.insertMissingCalculations
            logInfo ("Created " ++ show (length lst) ++ " new calculations")
    handleEvent Source.Calculations = do
        calcM <- dbRun Calc.startCalculation
        case calcM of
            Nothing ->
                logInfo "'startCalculation' returned Nothing: no unprocessed calculations."
                -- TODO: purge IBuyGraph cache?
            Just calc -> do
                R.lift $ STM.atomically $ PQ.insert (Db.getRunId $ Calc.calcCalc calc) calc calculationQueue
                logInfo $ "Inserted calculation: " ++ show (Beam.pk $ Calc.calcCalc calc)

-- TODO
monitorDeadCalculations :: NominalDiffTime -> IO ()
monitorDeadCalculations sleepTime = do
    -- add dead calculations to calculations-queue
    -- sleep for x seconds
    threadDelay (round $ sleepTime * 1e6)
    return ()
