{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Query.Liquidity
( selectQuantities
, LiquidityData(..)
, PathQty.Int64
, selectTestPathsSingle
, TestPathsSingleRes
)
where

import Internal.Prelude

import App.Orphans ()
import qualified App.Util
import Database
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run
import qualified Schema.Currency as Currency
import qualified Schema.Venue as Venue
import qualified Schema.RunCurrency as RC
import qualified Schema.Calculation as Calc
import qualified Schema.Path as Path
import qualified Schema.PathPart as PathPart
import qualified Schema.PathQty as PathQty
import qualified Schema.CalculationParameter as CalcParam

import Database.Beam
import Database.Beam.Backend (IsSql2008BigIntDataTypeSyntax, BeamSqlBackendSyntax, SqlNull, BeamSqlBackend)
import Database.Beam.Backend.SQL.SQL92
import qualified Database.Beam.Postgres.Full as Pg
import qualified Database.Beam.Postgres as Pg
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning(runUpdateReturningList), MonadBeamInsertReturning(runInsertReturningList))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import OrderBook.Graph.Types (Currency)
import Database.Beam.Query.Internal (QNested)
import Data.Bifunctor (Bifunctor(first))


allQuantities runQ = do
    run <- runQ
    calc <- calcsForRun run
    pathQty <- qtysForCalc calc
    pure (run, calc, pathQty)

-- | Same as 'allQuantities' but ignoring national currencies
cryptoQuantities runQ = do
    res@(_, calc, _) <- allQuantities runQ
    guard_ $ not_ $
        Calc.calculationCurrency calc `in_` map (val_ . Currency.CurrencyId) numeraires
    pure res
  where
    numeraires =
        [ "USD"
        , "EUR"
        , "GBP"
        , "JPY"
        , "AUD"
        , "CAD"
        , "CHF"
        , "CNY"
        , "HKD"
        , "NZD"
        , "SEK"
        , "KRW"
        , "SGD"
        , "NOK"
        , "MXN"
        , "INR"
        , "RUB"
        , "ZAR"
        , "TRY"
        ]

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
        guard_ $ exists_ (calcsForRun run)
        -- no unfinished calculation exists for run
        guard_ $ not_ $ exists_ $ filter_
            (\calc -> Calc.calculationDurationSeconds calc ==. val_ Nothing)
            (calcsForRun run)
        pure run

quantities
    :: ( HasSqlEqualityCheck be Path.Int32
       , HasSqlEqualityCheck be Calc.Int32
       , HasSqlEqualityCheck be Text
       , HasSqlEqualityCheck be Double
       , HasSqlInTable be
       , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) PathQty.Int64
       , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Text
       , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Double
       , IsSql2008BigIntDataTypeSyntax (Sql92ExpressionCastTargetSyntax (Sql92UpdateExpressionSyntax (Sql92UpdateSyntax (BeamSqlBackendSyntax be))))
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
              -- NB: postgres converts the type of this column to "numeric".
              -- We need to cast this in order to avoid the following runtime error,
              --  which happens when beam tries to parse an Int64 from a "numeric".
              --
              -- BeamRowReadError {
              --    brreColumn = Just 6,
              --    brreError = ColumnTypeMismatch {
              --       ctmHaskellType = "Integer",
              --       ctmSQLType = "numeric",
              --       ctmMessage = "types incompatible"
              --    }
              -- }
            , fromMaybe_ (val_ 0) (sum_ $ PathQty.pathqtyQty pathQty) `cast_` bigint
            )
        )
        (quantities' runQ numeraireM slippageM)

getSymbol (Currency.CurrencyId symbol) = symbol

quantities' runQ numeraireM slippageM = do
    (run, calc, pathQty) <- cryptoQuantities runQ
    forM_ numeraireM $ \numeraire -> guard_ $ Calc.calculationNumeraire calc ==. val_ (mkSymbol numeraire)
    forM_ slippageM $ \slippage -> guard_ $ Calc.calculationSlippage calc ==. val_ slippage
    pure (run, calc, pathQty)

-- runCalcs runQ numeraireM slippageM = do
--     run <- runQ
--     let calcsQuery = do
--             calc <- calcsForRun run
--             forM_ numeraireM $ \numeraire -> guard_ $ Calc.calculationNumeraire calc ==. val_ (mkSymbol numeraire)
--             forM_ slippageM $ \slippage -> guard_ $ Calc.calculationSlippage calc ==. val_ slippage
--             pure (Calc.calculationId calc, Calc.calculationCurrency calc)
--     pure (run, Pg.arrayOf_ calcsQuery)

-- test123 runQ numeraireM slippageM =
--     runSelectReturningList $ select $
--         runCalcs runQ numeraireM slippageM

-- data CalcTest f = CalcTest
--     { blahInt32 :: C f Path.Int32
--     , blahSlippage :: C f Double
--     }

mkSymbol :: Currency -> Currency.CurrencyId
mkSymbol symbol = Currency.CurrencyId (toS symbol)

testPathsSingle runQ numeraireM slippageM currency = do
    (run, calc, pathQty) <- quantities' runQ numeraireM slippageM
    path <- all_ $ paths liquidityDb
    guard_ $ pk path ==. PathQty.pathqtyPath pathQty
    path_part <- partsForPath path
    -- TMP
    guard_ $ Calc.calculationCurrency calc ==. Currency.CurrencyId (val_ $ toS currency)
    -- TMP
    return (run, (calc, ((pathQty, path), path_part)))

type TestPathsSingleRes = [(Run.Run, [(Calc.Calculation, [((PathQty.PathQty, Path.Path), Text)])])]

selectTestPathsSingle
    :: Currency
    -> Maybe Currency
    -> Maybe Double
    -> Pg.Pg TestPathsSingleRes
selectTestPathsSingle currency numeraireM slippageM = fmap convert $
    runSelectReturningList $ select $
        testPathsSingle (all_ $ runs liquidityDb) numeraireM slippageM currency
  where
    convert = map (fmap (map (fmap (map (fmap $ prettyPathParts (toS currency)) . fromPathList)) . fromCalcList)) . fromRunList

    fromRunList :: [(Run.Run, (Calc.Calculation, ((PathQty.PathQty, Path.Path), PathPart.PathPart))  )]
                -> [(Run.Run, [(Calc.Calculation, ((PathQty.PathQty, Path.Path), PathPart.PathPart))] )]
    fromRunList = groupNestByFst

    fromCalcList :: [(Calc.Calculation, ((PathQty.PathQty, Path.Path), PathPart.PathPart))]
                 -> [(Calc.Calculation, [((PathQty.PathQty, Path.Path), PathPart.PathPart)])]
    fromCalcList = groupNestByFst

    fromPathList :: [((PathQty.PathQty, Path.Path), PathPart.PathPart)]
                 -> [((PathQty.PathQty, Path.Path), [PathPart.PathPart])]
    fromPathList = groupNestByFst

    groupNestByFst :: Ord (UsingId b1) => [(b1, b2)] -> [(b1, [b2])]
    groupNestByFst resLst = map (first getUsingId) $ groupNest fst snd (map (first UsingId) resLst)

prettyPathParts
    :: Text -- start currency
    -> [PathPart.PathPart]
    -> Text -- path description
prettyPathParts start ppLst =
    let venueArrowTo pp = T.concat --     --bitfinex--> BTC
            [ "--"
            , getVenue (PathPart.pathpartVenue pp)
            , "--> "
            , getSymbol (PathPart.pathpartCurrency pp)
            ]
        getVenue (Venue.VenueId venueTxt) = venueTxt
    in start <> T.intercalate " " (map venueArrowTo $ sortOn PathPart.pathpartIndex ppLst)

newtype UsingId a = UsingId { getUsingId :: a }

instance Eq (UsingId Calc.Calculation) where
    UsingId a1 == UsingId a2 =
        Calc.calculationId a1 == Calc.calculationId a2

instance Ord (UsingId Calc.Calculation) where
    UsingId a1 `compare` UsingId a2 =
        Calc.calculationId a1 `compare` Calc.calculationId a2

instance Eq (UsingId Run.Run) where
    UsingId a1 == UsingId a2 =
        Run.runId a1 == Run.runId a2

instance Ord (UsingId Run.Run) where
    UsingId a1 `compare` UsingId a2 =
        Run.runId a1 `compare` Run.runId a2

instance Eq (UsingId (PathQty.PathQty, Path.Path)) where
    UsingId (_, a1) == UsingId (_, a2) =
        Path.pathId a1 == Path.pathId a2

instance Ord (UsingId (PathQty.PathQty, Path.Path)) where
    UsingId (_, a1) `compare` UsingId (_, a2) =
        Path.pathId a1 `compare` Path.pathId a2

groupNest
    :: Ord key
    => (a -> key)
    -> (a -> b)
    -> [a]
    -> [(key, [b])]
groupNest groupF nestF lst' =
    map (\lst -> (groupF $ head lst, map nestF lst)) $
        groupOn groupF lst'
