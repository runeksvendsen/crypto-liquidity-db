{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-- crypto-liquidity-db
import App.Monad (lift)
import qualified App.RunCalc
import Schema.Calculation (CalculationT(..))
import qualified Schema.Calculation
import qualified Schema.Currency
import qualified Query.Calculations
import qualified App.Main.Util
import qualified App.Monad

-- crypto-orderbook-db
import qualified CryptoDepth.OrderBook.Db.Insert        as Insert
import qualified CryptoDepth.OrderBook.Db.Monad         as Db
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run

-- beam-core
import Database.Beam.Backend (SqlSerial(SqlSerial))

-- time
import qualified Data.Time.Clock                        as Clock



main :: IO ()
main = putStrLn "Test suite not yet implemented"

runCalcTest bookList pool = App.Monad.withDbConn $ \conn -> do

    dummyTime <- lift Clock.getCurrentTime
    runId <- lift $ storeBooks dummyTime conn bookList
    let calc = mkCalc dummyTime runId
    paths <- App.RunCalc.runInsertCalculation calc
    return ()
 where
    -- runInsertCalc =
    currency = "BTC"
    numeraire = "USD"
    mkCalc dummyTime runId = Query.Calculations.Calculation (mkCalc' dummyTime runId) numeraire currency
    mkCalc' dummyTime runId =
        Calculation
            { calculationId = SqlSerial 1
            , calculationRun = runId
            , calculationCurrency = Schema.Currency.CurrencyId "BTC"
            , calculationNumeraire = Schema.Currency.CurrencyId "USD"
            , calculationSlippage = 0.5
            , calculationCreationTime = dummyTime
            , calculationStartTime = Nothing
            , calculationDurationSeconds = Nothing
            }

storeBooks dummyTime conn bookList = do
    (runId, _) <- Db.runDb conn $
        Insert.storeRun dummyTime dummyTime (map (dummyTime,) bookList)
    return runId




--- Util

withConn = App.Main.Util.withDbPool App.Main.Util.LevelDebug