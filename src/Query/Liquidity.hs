{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Query.Liquidity
( selectQuantities
, LiquidityData(..)
, PathQty.Word64
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
    (run, calc, pathQty) <- allQuantities (all_ $ runs liquidityDb)
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

-- allQuantities ::
--     ( HasSqlEqualityCheck be Run.Word32
--     , HasSqlEqualityCheck be Calc.Int32
--     )
--     => Q be LiquidityDb s
--         ( Run.RunT (QExpr be s)
--         , Calc.CalculationT (QExpr be s)
--         , PathQty.PathQtyT (QExpr be s)
--         )
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
    , ldQty :: PathQty.Word64
    } deriving (Generic)

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
        runSelectReturningList $ select $
            quantities (all_ $ runs liquidityDb) currencies fromM toM numeraireM slippageM limitM
  where
    mkLiquidityData (run, numeraire, slippage, currency, qty) = LiquidityData
        { ldRun = run
        , ldNumeraire = numeraire
        , ldSlippage = slippage
        , ldCurrency = currency
        , ldQty = qty
        }

quantities runQ currencies fromM toM numeraireM slippageM limitM =
    maybe (offset_ 0) (limit_ . fromIntegral) limitM $ -- apply LIMIT if present ("OFFSET 0" is a no-op)
    orderBy_ (\(run, numeraire, slippage, _, qty) ->
        (asc_ numeraire, asc_ slippage, asc_ $ Run.runId run, desc_ qty)
    ) $
    aggregate_
        (\(run, calcNumeraire, calcSlippage, calcCurrency, pathQty) ->
            ( group_ run
            , group_ calcNumeraire
            , group_ calcSlippage
            , group_ calcCurrency
            , fromMaybe_ (val_ 0) $ sum_ pathQty
            )
        )
        (quantities' runQ currencies fromM toM numeraireM slippageM)

quantities' runQ currencies fromM toM numeraireM slippageM = do
    (run, calc, pathQty) <- allQuantities runQ
    forM_ numeraireM $ \numeraire -> guard_ $ Calc.calculationNumeraire calc ==. val_ (mkSymbol numeraire)
    forM_ slippageM $ \slippage -> guard_ $ Calc.calculationSlippage calc ==. val_ slippage
    let currencySymbol = Calc.calculationCurrency calc
    -- if currencies == [] then "all currencies" else "match currencies in list"
    forM_ (NE.nonEmpty currencies) $ \currenciesNonEmpty ->
        guard_ $ currencySymbol `in_` map (val_ . mkSymbol) (NE.toList currenciesNonEmpty)
    pure
        ( run
        , getSymbol $ Calc.calculationNumeraire calc
        , Calc.calculationSlippage calc
        , getSymbol currencySymbol
        , PathQty.pathqtyQty pathQty
        )
  where
    mkSymbol :: Currency -> Currency.CurrencyId
    mkSymbol symbol = Currency.CurrencyId (toS symbol)
    getSymbol (Currency.CurrencyId symbol) = symbol

topXCryptos numeraire slippage count = do
    (_, _, _, currency, _) <- quantities
        newestRun [] Nothing Nothing (Just numeraire) (Just slippage) (Just count)
    return currency

newestRun :: BeamSqlBackend be => Q be LiquidityDb s (Run.RunT (QGenExpr QValueContext be s))
newestRun =
    limit_ 1 $
    orderBy_ (desc_ . Run.runTimeStart) $
    all_ (runs liquidityDb)
