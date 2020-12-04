module Query.Calculations
( selectRunCurrencyWithoutCalculation
)
where

import Database
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run
import qualified Schema.Currency as Currency
import qualified Schema.RunCurrency as RC
import qualified Schema.Calculation as Calc
import qualified Schema.CalculationParameter as CalcParam

import Database.Beam
import Database.Beam.Backend (BeamSqlBackend)


selectRunCurrencyWithoutCalculation ::
    ( MonadBeam be m
    , BeamSqlBackend be
    , HasQBuilder be
    , FromBackendRow be Run.Word32
    , FromBackendRow be Currency.Int32
    , HasSqlEqualityCheck be Run.Word32
    , HasSqlEqualityCheck be Currency.Int32
    )
    => m [RC.RunCurrency]
selectRunCurrencyWithoutCalculation =
    runSelectReturningList $ select runCurrencyWithNoCalculation

runCurrencyWithNoCalculation ::
    ( HasQBuilder be
    , HasSqlEqualityCheck be Run.Word32
    , HasSqlEqualityCheck be Currency.Int32
    )
    => Q be LiquidityDb s (RC.RunCurrencyT (QExpr be s))
runCurrencyWithNoCalculation = do
    rc  <- all_ $ runCurrencies liquidityDb
    calculation <- leftJoin_ (all_ $ calculations liquidityDb)
        (\calc -> Calc.calculationRun calc ==. RC.runCurrencyRun rc &&.
            Calc.calculationCurrency calc ==. RC.runCurrencyCurrency rc
        )
    guard_ (isNothing_ calculation)
    pure rc
