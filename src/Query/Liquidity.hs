{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Query.Liquidity
( selectQuantities
, LiquidityData(..)
, PathQty.Int64
)
where

import Internal.Prelude

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
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import OrderBook.Graph.Types (Currency)
import Database.Beam.Query.Internal (QNested)


-- |
getPaths ::
    ( HasSqlEqualityCheck be Path.Int32
    , HasSqlEqualityCheck be Calc.Int32
    , HasSqlEqualityCheck be Text
    , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Path.Int32
    , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Text
    )
    => Run.Int32   -- ^ run id
    -> Text -- ^ numeraire symbol
    -> Text -- ^ currency symbol
    -> Q be LiquidityDb s (QGenExpr QValueContext be s Double, QGenExpr QValueContext be s PathQty.Int64)
getPaths runId numeraireSymbol currencySymbol = do
    (run, calc, pathQty) <- allQuantities (all_ $ runs liquidityDb)
    path <- all_ (paths liquidityDb)
    guard_ $ PathQty.pathqtyPath pathQty `references_` path
    -- input args
    guard_ $
        Run.runId run ==. val_ (fromInteger $ fromIntegral runId)
        &&. Calc.calculationNumeraire calc ==. val_ (Currency.CurrencyId numeraireSymbol)
        &&. Calc.calculationCurrency calc ==. val_ (Currency.CurrencyId currencySymbol)
    pure (Calc.calculationSlippage calc, PathQty.pathqtyQty pathQty)

allQuantities runQ = do
    run <- runQ
    calc <- all_ (calculations liquidityDb)
    guard_ $ Calc.calculationRun calc `references_` run
    pathQty <- all_ (path_qtys liquidityDb)
    guard_ $ PathQty.pathqtyCalc pathQty `references_` calc
    pure (run, calc, pathQty)

data LiquidityData = LiquidityData
    { ldRun :: Run.Run
    , ldNumeraire :: Text
    , ldSlippage :: Double
    , ldCurrency :: Text
    , ldQty :: PathQty.Int64
    } deriving (Eq, Show, Generic)

-- |
selectQuantities
    :: [Currency]
    -> Maybe Run.UTCTime
    -> Maybe Run.UTCTime
    -> Maybe Currency
    -> Maybe Double
    -> Maybe Word
    -> Pg.Pg [LiquidityData]
selectQuantities currencies fromM toM numeraireM slippageM limitM =
    fmap (map mkLiquidityData) $
        runSelectReturningList $ select query
  where
    mkLiquidityData (run, numeraire, slippage, currency, qty) = LiquidityData
        { ldRun = run
        , ldNumeraire = numeraire
        , ldSlippage = slippage
        , ldCurrency = currency
        , ldQty = qty
        }
    query = case (limitM, numeraireM, slippageM, currencies) of
        (Just limit, Just numeraire, Just slippage, []) ->
            quantitiesLimit fromM toM numeraire slippage limit
        _ -> do
            res@(_, _, _, currency, _) <- quantities (all_ $ runs liquidityDb) numeraireM slippageM Nothing
            forM_ (NE.nonEmpty currencies) $ \currenciesNonEmpty ->
                guard_ $ currency `in_` map (val_ . toS) (NE.toList currenciesNonEmpty)
            pure res

quantitiesLimit fromM toM numeraire slippage limit = do
    res@(_, _, _, currency, _) <- quantities (all_ $ runs liquidityDb) (Just numeraire) (Just slippage) Nothing
    guard_ $ unknownAs_ False (currency ==*. anyOf_ topXCryptosNewestRun)
    pure res
  where
    topXCryptosNewestRun = do
        (_, _, _, currency, _) <- quantities
            newestFinishedRun (Just numeraire) (Just slippage) (Just limit)
        pure currency

    newestFinishedRun =
        limit_ 1 $
        orderBy_ (desc_ . Run.runTimeStart) $
        finishedRuns

    finishedRuns = do
        run <- all_ (runs liquidityDb)
        -- at least one calculation exists for run
        guard_ $ exists_ $ filter_
            (\calc -> Calc.calculationRun calc `references_` run)
            (all_ $ calculations liquidityDb)
        -- no unfinished calculation exists for run
        guard_ $ not_ $ exists_ $ filter_
            (\calc -> Calc.calculationRun calc `references_` run
                &&. Calc.calculationDurationSeconds calc ==. val_ Nothing
            )
            (all_ $ calculations liquidityDb)
        pure run

quantities
    :: ( HasSqlEqualityCheck be Path.Int32
       , HasSqlEqualityCheck be Calc.Int32
       , HasSqlEqualityCheck be Text
       , HasSqlEqualityCheck be Double
       , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) PathQty.Int64
       , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Text
       , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Double
       )
    => Q be LiquidityDb (QNested (QNested (QNested s))) (Run.RunT (QGenExpr QValueContext be (QNested (QNested (QNested s)))))
    -> Maybe Currency
    -> Maybe Double
    -> Maybe Word
    -> Q be LiquidityDb s
        ( Run.RunT (QGenExpr QValueContext be s)
        , QGenExpr QValueContext be s Text
        , QGenExpr QValueContext be s Double
        , QGenExpr QValueContext be s Text
        , QGenExpr QValueContext be s PathQty.Int64
        )
quantities runQ numeraireM slippageM limitM =
    maybe (offset_ 0) (limit_ . fromIntegral) limitM $ -- apply LIMIT if present ("OFFSET 0" is a no-op)
    orderBy_ (\(run, numeraire, slippage, _, qty) ->
        (asc_ numeraire, asc_ slippage, asc_ $ Run.runId run, desc_ qty)
    ) $
    aggregate_
        (\(run, calc, pathQty) ->
            ( group_ run
            , group_ (getSymbol $ Calc.calculationNumeraire calc)
            , group_ (Calc.calculationSlippage calc)
            , group_ $ getSymbol (Calc.calculationCurrency calc)
            , fromMaybe_ (val_ 0) $ sum_ (PathQty.pathqtyQty pathQty)
            )
        )
        (quantities' runQ numeraireM slippageM)
  where
    getSymbol (Currency.CurrencyId symbol) = symbol

quantities' runQ numeraireM slippageM = do
    (run, calc, pathQty) <- allQuantities runQ
    forM_ numeraireM $ \numeraire -> guard_ $ Calc.calculationNumeraire calc ==. val_ (mkSymbol numeraire)
    forM_ slippageM $ \slippage -> guard_ $ Calc.calculationSlippage calc ==. val_ slippage
    pure (run, calc, pathQty)

mkSymbol :: Currency -> Currency.CurrencyId
mkSymbol symbol = Currency.CurrencyId (toS symbol)
