{-# LANGUAGE NumericUnderscores #-}
module App.Main.ProcessCalc
( main
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


runServices :: Has DbConn r => AppM r ()
runServices = do
    a1 <- async processCalculationsPoll
    lift $ snd <$> Async.waitAny [a1]
  where
    processCalculationsPoll = forever $
        processCalculations >> lift (threadDelay 60_000_000)

main :: DbConn -> IO ()
main dbc = runAppM dbc $ do
    runServices
    logInfo "MAIN" "Done. Exiting..."

processCalculations :: Has DbConn r => AppM r ()
processCalculations = do
    now <- lift App.Util.currentTime
    calcM <- runDbRaw $ Calc.startCalculation now
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
        intercalate "/"
            [ show (Db.calculationRun dbCalc)
            , toS $ Calc.calcNumeraire calc
            , toS $ Calc.calcCrypto calc
            , printf "%f" (Db.calculationSlippage dbCalc)
            ]
