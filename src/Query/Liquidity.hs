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
, selectNewestRunAllLiquidity
, selectNewestRunAllPaths
, Query.Graph.GraphData
)
where

import Internal.Prelude

import qualified Query.Graph
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
import qualified Schema.PathSum as PathSum
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
import Protolude (headMay)
import qualified Data.Aeson as Json


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
            res@(_, currency, _) <- quantities (runsWithinTime fromM toM) numeraireM slippageM
            forM_ (NE.nonEmpty currencies) $ \currenciesNonEmpty ->
                guard_ $ currency `in_` map (val_ . toS) (NE.toList currenciesNonEmpty)
            pure res

runsWithinTime fromM toM = do
    run <- all_ (runs liquidityDb)
    forM_ fromM $ \fromTime -> guard_ $ Run.runTimeStart run >=. val_ fromTime
    forM_ toM $ \endTime -> guard_ $ Run.runTimeEnd run <=. val_ endTime
    pure run

quantitiesLimit fromM toM numeraire slippage limit = do
    res@(_, currency, _) <- quantities (runsWithinTime fromM toM) (Just numeraire) (Just slippage)
    guard_ $ unknownAs_ False (currency ==*. anyOf_ topXCryptos)
    pure res
  where
    topXCryptos = topCurrencies newest100Runs numeraire slippage (Just limit)

    newest100Runs = -- at most 100 (non-empty) runs within given time period
        limitQ (Just 100) $ orderBy_ (desc_ . Run.runId) $ do
            run <- runsWithinTime fromM toM
            guard_ $ nonEmptyRun run (Just numeraire) (Just slippage)
            pure run

topCurrencies runQ numeraire slippage limitM = fmap fst $ do
    limitQ limitM $
        orderBy_ (desc_ . snd) $
            aggregate_
                (\(currency, qty) ->
                    ( group_ currency
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
                    --
                    -- Beam issue: https://github.com/haskell-beam/beam/issues/545
                    , fromMaybe_ (val_ 0) (sum_ qty) `cast_` (bigint :: DataType Pg.Postgres PathQty.Int64)
                    )
                ) $ do
                    run <- runQ
                    calc <- finishedCryptoCalcsForRun run (Just numeraire) (Just slippage)
                    pathSum <- sumForCalc calc
                    let qtySum = PathSum.pathsumBuyQty pathSum + PathSum.pathsumSellQty pathSum
                    pure (getSymbol $ Calc.calculationCurrency calc, qtySum)

-- | Does the run have at least a single finished calculation?
nonEmptyRun run numeraireM slippageM = do
    exists_ $
        Calc.calculationId <$> finishedCalcsForRun run numeraireM slippageM

-- | A run with at least a single calculation, all of which are finished
finishedRun runQ numeraireM slippageM = do
    guard_ noUnfinishedCalcs
    guard_ $ exists_ $ do
        calc <- calcsForRun runQ
        whereNumeraireSlippage calc numeraireM slippageM
        pure $ Calc.calculationId calc
  where
    noUnfinishedCalcs =
        not_ $ exists_ $ do
            calc <- calcsForRun runQ
            whereNumeraireSlippage calc numeraireM slippageM
            guard_ $ isNothing_ $ Calc.calculationDurationSeconds calc
            pure $ Calc.calculationId calc

quantities
    :: Q Pg.Postgres LiquidityDb s (Run.RunT (QExpr Pg.Postgres s))
    -> Maybe Currency
    -> Maybe Double
    -> Q Pg.Postgres LiquidityDb s
        ( Run.RunT (QExpr Pg.Postgres s)
        , QExpr Pg.Postgres s Text
        , QExpr Pg.Postgres s PathSum.Int64
        )
quantities runQ numeraireM slippageM = do
    (run, calc, qtySum) <- runCalcQuantity
    pure (run, getSymbol (Calc.calculationCurrency calc), qtySum)
  where
    runCalcQuantity = do
        run <- runQ
        calc <- finishedCryptoCalcsForRun run numeraireM slippageM
        pathSum <- sumForCalc calc
        pure (run, calc, PathSum.pathsumBuyQty pathSum + PathSum.pathsumSellQty pathSum)

getSymbol (Currency.CurrencyId symbol) = symbol

whereNumeraireSlippageFinished calc numeraireM slippageM = do
    forM_ numeraireM $ \numeraire -> guard_ $ Calc.calculationNumeraire calc ==. val_ (mkSymbol numeraire)
    forM_ slippageM $ \slippage -> guard_ $ Calc.calculationSlippage calc ==. val_ slippage
    guard_ $ isJust_ $ Calc.calculationDurationSeconds calc

-- TODO: merge with "whereNumeraireSlippageFinished" somehow
whereNumeraireSlippage calc numeraireM slippageM = do
    forM_ numeraireM $ \numeraire -> guard_ $ Calc.calculationNumeraire calc ==. val_ (mkSymbol numeraire)
    forM_ slippageM $ \slippage -> guard_ $ Calc.calculationSlippage calc ==. val_ slippage

mkSymbol :: Currency -> Currency.CurrencyId
mkSymbol symbol = Currency.CurrencyId (toS symbol)

testPathsSingle runQ numeraire slippage currency = do
    (run, calc, pathQty) <- allQuantitiesPaths runQ
    path <- all_ $ paths liquidityDb
    guard_ $ pk path ==. PathQty.pathqtyPath pathQty
    guard_ $ Calc.calculationCurrency calc ==. Currency.CurrencyId (val_ $ toS currency)
    return (run, (calc, (pathQty, path)))
  where
    allQuantitiesPaths runQ' = do
        run <- runQ'
        calc <- finishedCalcsForRun run (Just numeraire) (Just slippage)
        pathQty <- qtysForCalc calc
        pure (run, calc, pathQty)

type TestPathsSingleRes = (Run.Run, [(Calc.Calculation, [(PathQty.PathQty, Path.Path)])])

selectTestPathsSingle
    :: Run.RunId
    -> Currency
    -> Double
    -> Currency
    -> Pg.Pg (Maybe TestPathsSingleRes)
selectTestPathsSingle runId numeraire slippage currency = fmap (headMay . convert) $
    runSelectReturningList $ select $
        orderBy_ (desc_ . getPathQty) $
        testPathsSingle runQ numeraire slippage currency
  where
    runQ = do
        run <- all_ (runs liquidityDb)
        guard_ $ val_ runId `references_` run
        pure run

    getPathQty (_, (_, (pathQty, _))) = PathQty.pathqtyQty pathQty

    convert = map (fmap fromCalcList) . fromRunList

    fromRunList :: [(Run.Run, (Calc.Calculation, (PathQty.PathQty, Path.Path))  )]
                -> [(Run.Run, [(Calc.Calculation, (PathQty.PathQty, Path.Path))] )]
    fromRunList = groupNestByFst

    fromCalcList :: [(Calc.Calculation, (PathQty.PathQty, Path.Path))]
                 -> [(Calc.Calculation, [(PathQty.PathQty, Path.Path)])]
    fromCalcList = groupNestByFst

groupNestByFst :: Ord (UsingId b1) => [(b1, a)] -> [(b1, [a])]
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
    maybe (offset_ 0) (limit_ . fromIntegral) (limitM :: Maybe Word)

finishedCalcsForRun run numeraireM slippageM = do
    calc <- calcsForRun run
    whereNumeraireSlippageFinished calc numeraireM slippageM
    pure calc

finishedCryptoCalcsForRun run numeraireM slippageM = do
    calc <- finishedCalcsForRun run numeraireM slippageM
    guard_ $ isCrypto calc
    pure calc

selectNewestRunAllLiquidity numeraireM slippageM offsetM limitM =
    runSelectReturningList $ select $ newestRunAllLiquidity numeraireM slippageM offsetM limitM

newestRunAllLiquidity
    :: ( HasQBuilder be
       , HasSqlEqualityCheck be Text
       , HasSqlEqualityCheck be Double
       , HasSqlEqualityCheck be Path.Int32
       , HasSqlInTable be
       , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Text
       , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Double
       , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) PathSum.Int64
       )
    => Maybe Currency
    -> Maybe Double
    -> Maybe Integer
    -> Maybe Integer
    -> Q be LiquidityDb s
        ( Run.RunT (QGenExpr QValueContext be s)
        , QGenExpr QValueContext be s Text
        , QGenExpr QValueContext be s PathSum.Int64
        , QGenExpr QValueContext be s PathSum.Int64
        )
newestRunAllLiquidity numeraireM slippageM offsetM limitM =
    limit_ (fromMaybe 50 limitM) $
        offset_ (fromMaybe 0 offsetM) $
            orderBy_ (\(_, _, buyQty, sellQty) -> desc_ (buyQty + sellQty))
                newestRunCalcs
  where
    newestRunCalcs = do
        run <- newestNonEmptyRun
        calc <- finishedCryptoCalcsForRun run numeraireM slippageM
        pathSum <- sumForCalc calc
        pure ( run
             , getSymbol $ Calc.calculationCurrency calc
             , PathSum.pathsumBuyQty pathSum
             , PathSum.pathsumSellQty pathSum
             )
    newestNonEmptyRun =
        limit_ 1 $
            orderBy_ (desc_ . Run.runId) $ do
                run <- all_ (runs liquidityDb)
                guard_ $ nonEmptyRun run numeraireM slippageM
                pure run

newestFinishedRun numeraireM slippageM =
    limit_ 1 $
        orderBy_ (desc_ . Run.runId) $ do
            run <- all_ (runs liquidityDb)
            finishedRun run numeraireM slippageM
            pure run

selectNewestRunAllPaths
    :: Currency
    -> Double
    -> Maybe Word
    -> Pg.Pg (Maybe Query.Graph.GraphData)
selectNewestRunAllPaths numeraire slippage limitM = do
    cryptoQtys <- map (first toS) <$> runSelectReturningList (select $ newestRunCryptoQtys numeraire slippage)
    fmap (fmap (Query.Graph.toGraphData limitM cryptoQtys) . headMay . groupNestByFst) $
        runSelectReturningList $ select $ newestRunAllPaths numeraire slippage

newestRunCryptoQtys
    :: Currency
    -> Double
    -> Q Pg.Postgres LiquidityDb s
        ( QGenExpr QValueContext Pg.Postgres s Text
        , QGenExpr QValueContext Pg.Postgres s PathSum.Int64
        )
newestRunCryptoQtys numeraire slippage = do
    run <- newestFinishedRun (Just numeraire) (Just slippage)
    calc <- finishedCalcsForRun run (Just numeraire) (Just slippage)
    pathSum <- sumForCalc calc
    let qtySum = PathSum.pathsumBuyQty pathSum + PathSum.pathsumSellQty pathSum
    pure (getSymbol $ Calc.calculationCurrency calc, qtySum)

newestRunAllPaths
    :: Currency
    -> Double
    -> Q Pg.Postgres LiquidityDb s
        ( Run.RunT (QGenExpr QValueContext Pg.Postgres s)
        ,   ( QGenExpr QValueContext Pg.Postgres s Text
            , QGenExpr QValueContext Pg.Postgres s PathSum.Int64
            , Path.PathT (QExpr Pg.Postgres s)
            )
        )
newestRunAllPaths numeraire slippage = do
    run <- newestFinishedRun (Just numeraire) (Just slippage)
    calc <- finishedCryptoCalcsForRun run (Just numeraire) (Just slippage)
    let currencySymbol = getSymbol $ Calc.calculationCurrency calc
    (pathQty, path) <- qtyAndPath calc
    pure ( run
           , ( currencySymbol
             , PathQty.pathqtyQty pathQty
             , path
             )
         )
  where
    qtyAndPath calc = do
        pathQty <- qtysForCalc calc
        path <- all_ $ paths liquidityDb
        guard_ $ pk path ==. PathQty.pathqtyPath pathQty
        pure (pathQty, path)

instance Json.ToJSON LiquidityData where
    toJSON = Json.genericToJSON App.Util.prefixOptions
instance Json.FromJSON LiquidityData where
    parseJSON = Json.genericParseJSON App.Util.prefixOptions
