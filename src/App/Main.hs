{-# LANGUAGE OverloadedStrings #-}
module App.Main
( main
, Config(..)
)
where

import Internal.Prelude
import qualified App.Source as Source
import qualified Schema.Calculation as Db
import qualified Query.Calculations as Calc
import qualified Query.RunCurrencies as Query
import App.Orphans ()

import Data.Time.Clock (NominalDiffTime)
import qualified Control.Concurrent.STM.TQueue as STM
import qualified Control.Monad.STM as STM
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.Async as Async
import qualified Database.PostgreSQL.Simple.Notification as PgNotify
import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam as Beam
import qualified Control.Monad.Reader as R
import GHC.Conc (threadDelay)


{-# NOINLINE calculationQueue #-}
calculationQueue :: STM.TQueue Db.Calculation
calculationQueue = unsafePerformIO STM.newTQueueIO


type AppM = R.ReaderT Config IO

logInfo :: String -> AppM ()
logInfo = R.lift . putStrLn

runAppM :: Config -> R.ReaderT Config m a -> m a
runAppM = flip R.runReaderT

withDbConn :: (Pg.Connection -> AppM a) -> AppM a
withDbConn f = do
    cfg <- R.ask
    let withConn f = error "TODO"
    withConn cfg

dbRun :: Pg.Pg a -> AppM a
dbRun appM = do
    withDbConn $ \conn -> R.lift $ Pg.runBeamPostgresDebug putStrLn conn appM

main :: Config -> IO ()
main cfg = do
    a1 <- Async.async processCalculations
    a2 <- Async.async $ monitorDeadCalculations (cfgDeadMonitorInterval cfg)
    a3 <- Async.async $ runAppM cfg notificationsListen
    -- TODO: wait until 'notificationsListen' is listening for notifications
    runAppM cfg insertMissingRunCurrencies
    snd <$> Async.waitAny [a1, a2, a3]

insertMissingRunCurrencies :: AppM ()
insertMissingRunCurrencies = do
    res <- dbRun Query.insertMissingRunCurrencies
    logInfo $ "Inserted run currencies: " ++ show res


processCalculations :: IO ()
processCalculations = do
    calculation <- STM.atomically $ STM.readTQueue calculationQueue
    -- TODO: process calculation
    return ()
  where
    calculationParameters cfg = do
        numeraire <- cfgNumeraires cfg
        slippage <- cfgSlippages cfg
        return (numeraire, slippage)


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
            Just calc -> do
                R.lift $ STM.atomically $ STM.writeTQueue calculationQueue calc
                logInfo $ "Inserted calculation: " ++ show (Beam.pk calc)

monitorDeadCalculations :: NominalDiffTime -> IO ()
monitorDeadCalculations sleepTime = do
    -- add dead calculations to calculations-queue
    -- sleep for x seconds
    threadDelay (round $ sleepTime * 1e6)
    return ()

-- |
data Config = Config
    { cfgNumeraires :: [Text]
    , cfgSlippages :: [Double]
    , cfgMaxCalculationTime :: NominalDiffTime
      -- ^ the amount of time after which a calculation is considered stalled/dead
    , cfgDeadMonitorInterval :: NominalDiffTime
      -- ^ restart dead calculations this often
    }
