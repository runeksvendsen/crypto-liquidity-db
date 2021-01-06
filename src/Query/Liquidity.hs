module Query.Liquidity
( quantities
, PathQty.Word64
)
where

import App.Orphans ()
import qualified App.Util
import Database
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run
import qualified Schema.Currency as Currency
import qualified Schema.RunCurrency as RC
import qualified Schema.Calculation as Calc
import qualified Schema.Path as Path
import qualified Schema.PathPart as PathPart
import qualified Schema.PathQty as PathQty
import qualified Schema.CalculationParameter as CalcParam

import Database.Beam
import Database.Beam.Backend (BeamSqlBackendSyntax, SqlNull, BeamSqlBackend)
import Database.Beam.Backend.SQL.SQL92
import qualified Database.Beam.Postgres.Full as Pg
import qualified Database.Beam.Postgres as Pg
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning(runUpdateReturningList), MonadBeamInsertReturning(runInsertReturningList))
import Data.Maybe (fromMaybe)
import Internal.Prelude (Text)
import OrderBook.Graph.Types (Currency)


-- |
getPaths ::
    ( HasSqlEqualityCheck be Path.Word32
    , HasSqlEqualityCheck be Calc.Int32
    , HasSqlEqualityCheck be Text
    , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Path.Word32
    , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Text
    )
    => Run.Word32   -- ^ run id
    -> Text -- ^ numeraire symbol
    -> Text -- ^ currency symbol
    -> Q be LiquidityDb s (QGenExpr QValueContext be s Double, QGenExpr QValueContext be s PathQty.Word64)
getPaths runId numeraireSymbol currencySymbol = do
    (run, calc, pathQty) <- allQuantities
    path <- all_ (paths liquidityDb)
    guard_ $ PathQty.pathqtyPath pathQty `references_` path
    pathPart <- partsForPath path
    -- input args
    guard_ $
        Run.runId run ==. val_ (fromInteger $ fromIntegral runId)
        &&. Calc.calculationNumeraire calc ==. val_ (Currency.CurrencyId numeraireSymbol)
        &&. Calc.calculationCurrency calc ==. val_ (Currency.CurrencyId currencySymbol)
    let lel = Path.pathStart path
            -- PathPart.pathpartIndex
            -- PathPart.pathpartVenue
            -- PathPart.pathpartCurrency
    pure (Calc.calculationSlippage calc, PathQty.pathqtyQty pathQty)

allQuantities ::
    ( HasSqlEqualityCheck be Run.Word32
    , HasSqlEqualityCheck be Calc.Int32
    )
    => Q be LiquidityDb s
        ( Run.RunT (QExpr be s)
        , Calc.CalculationT (QExpr be s)
        , PathQty.PathQtyT (QExpr be s)
        )
allQuantities = do
    run <- all_ (runs liquidityDb)
    calc <- all_ (calculations liquidityDb)
    guard_ $ Calc.calculationRun calc `references_` run
    pathQty <- all_ (path_qtys liquidityDb)
    guard_ $ PathQty.pathqtyCalc pathQty `references_` calc
    pure (run, calc, pathQty)

quantities' currencies fromM toM = do
    (run, calc, pathQty) <- allQuantities
    pure
        ( run
        , Calc.calculationNumeraire calc
        , Calc.calculationSlippage calc
        , getSymbol $ Calc.calculationCurrency calc
        , PathQty.pathqtyQty pathQty
        )
  where
    getSymbol (Currency.CurrencyId symbol) = symbol

quantities currencies fromM toM =
    aggregate_
        (\(run, calcNumeraire, calcSlippage, calcCurrency, pathQty) ->
            (group_ (pk run, calcNumeraire, calcSlippage, calcCurrency), sum_ pathQty)
        )
        (quantities' currencies fromM toM)
