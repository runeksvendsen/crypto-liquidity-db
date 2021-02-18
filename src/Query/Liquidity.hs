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
, prettyPathParts
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
import qualified Data.Vector as Vec


isCrypto calc = do
    not_ $ Calc.calculationCurrency calc `in_` map (val_ . Currency.CurrencyId) numeraires
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
    , ldRunId :: Run.RunId
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
    mkLiquidityData (run, currency, qty) = LiquidityData
        { ldRun = run
        , ldRunId = pk run
        , ldCurrency = currency
        , ldQty = qty
        }
    query = case (limitM, numeraireM, slippageM, currencies) of
        (Just limit, Just numeraire, Just slippage, []) ->
            quantitiesLimit fromM toM numeraire slippage limit
        _ -> do
            res@(_, currency, _) <- quantities (runsWithinTime numeraireM slippageM fromM toM) numeraireM slippageM Nothing
            forM_ (NE.nonEmpty currencies) $ \currenciesNonEmpty ->
                guard_ $ currency `in_` map (val_ . toS) (NE.toList currenciesNonEmpty)
            pure res

runsWithinTime numeraireM slippageM fromM toM = do
    run <- case (numeraireM, slippageM) of
            (Just numeraire, Just slippage) -> nonEmptyRuns numeraire slippage
            _ -> all_ (runs liquidityDb)
    forM_ fromM $ \fromTime -> guard_ $ Run.runTimeStart run >=. val_ fromTime
    forM_ toM $ \endTime -> guard_ $ Run.runTimeEnd run <=. val_ endTime
    pure run

quantitiesLimit fromM toM numeraire slippage limit = do
    res@(_, currency, _) <- quantities (runsWithinTime (Just numeraire) (Just slippage) fromM toM) (Just numeraire) (Just slippage) Nothing
    guard_ $ unknownAs_ False (currency ==*. anyOf_ topXCryptos)
    pure res
  where
    topXCryptos = topCurrencies (Just limit)

    topCurrencies limitM = fmap fst $ do
        limitQ limitM $
            orderBy_ (desc_ . snd) $
                aggregate_
                    (\(currency, qty) ->
                        ( group_ currency
                        , fromMaybe_ (val_ 0) (sum_ qty) `cast_` (bigint :: DataType Pg.Postgres PathQty.Int64)
                        )
                    ) $ do
                        -- get at most 100 runs within given time period
                        run <- limitQ (Just 100) $ orderBy_ (desc_ . Run.runId) $
                            runsWithinTime (Just numeraire) (Just slippage) fromM toM
                        calc <- calcsForRun run
                        guard_ $ isCrypto calc
                        pathQty <- qtysForCalc calc
                        pure (getSymbol $ Calc.calculationCurrency calc, PathQty.pathqtyQty pathQty)

-- | Runs with at least a single PathQty
nonEmptyRuns :: Currency -> Double -> Q Pg.Postgres LiquidityDb s (Run.RunT (QExpr Pg.Postgres s))
nonEmptyRuns numeraire slippage = do
    run <- all_ (runs liquidityDb)
    guard_ $ exists_ $ do
        calc <- calcsForRun run
        guard_ $
            Calc.calculationNumeraire calc ==. val_ (mkSymbol numeraire)
            &&. Calc.calculationSlippage calc ==. val_ slippage
        pathQty <- qtysForCalc calc
        pure $ PathQty.pathqtyQty pathQty
    pure run

quantities
    :: Q Pg.Postgres LiquidityDb (QNested (QNested s)) (Run.RunT (QExpr Pg.Postgres (QNested (QNested s))))
    -> Maybe Currency
    -> Maybe Double
    -> Maybe Word
    -> Q Pg.Postgres LiquidityDb s
        ( Run.RunT (QGenExpr QValueContext Pg.Postgres s)
        , QGenExpr QValueContext Pg.Postgres s Text
        , QGenExpr QValueContext Pg.Postgres s PathQty.Int64
        )
quantities runQ numeraireM slippageM limitM =
    maybe (offset_ 0) (limit_ . fromIntegral) limitM $ -- apply LIMIT if present ("OFFSET 0" is a no-op)
    aggregate_
        (\(run, calc, pathQty) ->
            ( group_ run
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
            , fromMaybe_ (val_ 0) (sum_ pathQty) `cast_` bigint
            )
        ) $
        do
            res@(_, calc, _) <- allQuantities
            whereNumeraireSlippage calc numeraireM slippageM
            pure res
  where
    allQuantities = do
        run <- runQ
        calc <- calcsForRun run
        pathQty <- qtysForCalc calc
        pure (run, calc, PathQty.pathqtyQty pathQty)

getSymbol (Currency.CurrencyId symbol) = symbol

whereNumeraireSlippage calc numeraireM slippageM = do
    forM_ numeraireM $ \numeraire -> guard_ $ Calc.calculationNumeraire calc ==. val_ (mkSymbol numeraire)
    forM_ slippageM $ \slippage -> guard_ $ Calc.calculationSlippage calc ==. val_ slippage

mkSymbol :: Currency -> Currency.CurrencyId
mkSymbol symbol = Currency.CurrencyId (toS symbol)

testPathsSingle runQ numeraireM slippageM currency = do
    (run, calc, pathQty) <- allQuantitiesPaths runQ
    whereNumeraireSlippage calc numeraireM slippageM
    path <- all_ $ paths liquidityDb
    guard_ $ pk path ==. PathQty.pathqtyPath pathQty
    -- TMP
    guard_ $ Calc.calculationCurrency calc ==. Currency.CurrencyId (val_ $ toS currency)
    -- TMP
    return (run, (calc, (pathQty, path)))
  where
    allQuantitiesPaths runQ' = do
        run <- runQ'
        calc <- calcsForRun run
        pathQty <- qtysForCalc calc
        pure (run, calc, pathQty)

type TestPathsSingleRes = [(Run.Run, [(Calc.Calculation, [(PathQty.PathQty, Text)])])]

selectTestPathsSingle
    :: Currency
    -> Maybe Currency
    -> Maybe Double
    -> Maybe Run.RunId
    -> Pg.Pg TestPathsSingleRes
selectTestPathsSingle currency numeraireM slippageM runM = fmap convert $
    runSelectReturningList $ select $
        orderBy_ getPathQty $
        testPathsSingle runQ numeraireM slippageM currency
  where
    runQ = do
        run <- all_ (runs liquidityDb)
        forM_ runM $ \runId ->
            guard_ $ val_ runId `references_` run
        pure run

    getPathQty (run, (calc, (pathQty, path))) = desc_ $ PathQty.pathqtyQty pathQty

    convert = map (fmap (map (fmap (map mkPrettyPathParts)) . fromCalcList)) . fromRunList

    mkPrettyPathParts (pathQty, path) =
        (pathQty, prettyPathParts path)

    fromRunList :: [(Run.Run, (Calc.Calculation, (PathQty.PathQty, Path.Path))  )]
                -> [(Run.Run, [(Calc.Calculation, (PathQty.PathQty, Path.Path))] )]
    fromRunList = groupNestByFst

    fromCalcList :: [(Calc.Calculation, (PathQty.PathQty, Path.Path))]
                 -> [(Calc.Calculation, [(PathQty.PathQty, Path.Path)])]
    fromCalcList = groupNestByFst

    groupNestByFst :: Ord (UsingId b1) => [(b1, b2)] -> [(b1, [b2])]
    groupNestByFst resLst = map (first getUsingId) $ groupNest fst snd (map (first UsingId) resLst)

prettyPathParts
    :: Path.Path
    -> Text -- path description
prettyPathParts path =
    let ppLst = zip (Vec.toList $ Path.pathVenues path) (Vec.toList $ Path.pathCurrencys path)
        venueArrowTo (venue, currency) = T.concat --     --bitfinex--> BTC
            [ "--"
            , venue
            , "--> "
            , currency
            ]
    in T.intercalate " " (getSymbol (Path.pathStart path) : map venueArrowTo ppLst)

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

limitQ limitM =
    maybe (offset_ 0) (limit_ . fromIntegral) limitM
