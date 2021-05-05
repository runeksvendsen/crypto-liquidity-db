{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Query.Liquidity
( selectQuantities
, LiquidityData(..)
, SingleRun(..)
, PathQty.Int64
, selectTestPathsSingle
, TestPathsSingleRes
, prettyPathParts
, selectNewestFinishedRunId
, selectNewestRunAllLiquidity
, selectSpecificRunAllLiquidity
, selectNewestRunAllPaths
, Query.Graph.GraphData
)
where

import Internal.Prelude

import qualified Query.Graph
import Query.Config.Config (numeraires)
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
import Database.Beam.Backend (SqlSerial(SqlSerial)) -- TMP!!!!


isCrypto calc = do
    not_ $ Calc.calculationCurrency calc `in_` map (val_ . Currency.CurrencyId) numeraires

data LiquidityData = LiquidityData
    { ldRun :: Run.Run
    , ldRunId :: Run.RunId
    , ldCurrency :: Text
    , ldQty :: PathQty.Int64
    , ldQtyBuy :: PathQty.Int64
    , ldQtySell :: PathQty.Int64
    } deriving (Eq, Show, Generic)

-- | A single run
data SingleRun
    = SpecificRun Run.RunId
    | NewestFinishedRun

runsWithinRange
    :: Currency
    -> Double
    -> Maybe Run.UTCTime -- ^ from
    -> Either SingleRun Run.UTCTime -- ^ to
    -> Q Pg.Postgres LiquidityDb s (Run.RunT (QExpr Pg.Postgres s))
runsWithinRange _ _ fromM (Right to) =
    runsWithinTime' fromM (Just to)
runsWithinRange numeraire slippage fromM (Left singleRun) = do
    runId <- case singleRun of
        NewestFinishedRun -> Run.runId <$> newestFinishedRun numeraire slippage
        SpecificRun (Run.RunId runId') -> return $ val_ runId'
    run <- all_ (runs liquidityDb)
    guard_ $ Run.runId run <=. runId
    forM_ fromM (runStartTime run)
    pure run

runStartTime :: Run.RunT (QExpr Pg.Postgres s) -> Run.UTCTime -> Q Pg.Postgres LiquidityDb s ()
runStartTime run fromTime = guard_ $ Run.runTimeStart run >=. val_ fromTime

runEndTime :: Run.RunT (QExpr Pg.Postgres s) -> Run.UTCTime -> Q Pg.Postgres LiquidityDb s ()
runEndTime run endTime = guard_ $ Run.runTimeEnd run <=. val_ endTime

runsWithinTime'
    :: Maybe Run.UTCTime
    -> Maybe Run.UTCTime
    -> Q Pg.Postgres LiquidityDb s (Run.RunT (QExpr Pg.Postgres s))
runsWithinTime' fromM toM = do
    run <- all_ (runs liquidityDb)
    forM_ fromM (runStartTime run)
    forM_ toM (runEndTime run)
    pure run

-- |
selectQuantities
    :: [Currency]
    -> Currency
    -> Double
    -> Maybe Run.UTCTime -- ^ from
    -> Either SingleRun Run.UTCTime -- ^ to
    -> Maybe Word
    -> Pg.Pg [LiquidityData]
selectQuantities currencies numeraire slippage fromM runOrTo limitM =
    fmap (map mkLiquidityData) $
        runSelectReturningList $ select (orderBy_ runTimeStart query)
  where
    runTimeStart (run, _, _) = asc_ $ Run.runTimeStart run
    mkLiquidityData (run, currency, (qty, qtyBuy, qtySell)) = LiquidityData
        { ldRun = run
        , ldRunId = pk run
        , ldCurrency = currency
        , ldQty = qty
        , ldQtyBuy = qtyBuy
        , ldQtySell = qtySell
        }
    query = case (limitM, currencies) of
        (Just limit, []) ->
            quantitiesLimit fromM runOrTo numeraire slippage limit
        _ -> do
            let runQ = runsWithinRange numeraire slippage fromM runOrTo
            res@(_, currency, _) <- quantities runQ (Just numeraire) (Just slippage)
            forM_ (NE.nonEmpty currencies) $ \currenciesNonEmpty ->
                guard_ $ currency `in_` map (val_ . toS) (NE.toList currenciesNonEmpty)
            pure res

quantitiesLimit fromM runOrTo numeraire slippage limit = do
    res@(_, currency, _) <- quantities (runsWithinRange numeraire slippage fromM runOrTo) (Just numeraire) (Just slippage)
    guard_ $ unknownAs_ False (currency ==*. anyOf_ topXCryptos)
    pure res
  where
    topXCryptos = topCurrencies newest100Runs numeraire slippage (Just limit)

    newest100Runs = -- at most 100 (non-empty) runs within given time period
        limitQ (Just 100) $ orderBy_ (desc_ . Run.runId) $ do
            run <- runsWithinRange numeraire slippage fromM runOrTo
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
        , ( QExpr Pg.Postgres s PathSum.Int64
          , QExpr Pg.Postgres s PathSum.Int64
          , QExpr Pg.Postgres s PathSum.Int64
          )
        )
quantities runQ numeraireM slippageM = do
    (run, calc, qtySum) <- runCalcQuantity
    pure (run, getSymbol (Calc.calculationCurrency calc), qtySum)
  where
    runCalcQuantity = do
        run <- runQ
        calc <- finishedCryptoCalcsForRun run numeraireM slippageM
        pathSum <- sumForCalc calc
        pure ( run
             , calc
             , ( PathSum.pathsumBuyQty pathSum + PathSum.pathsumSellQty pathSum
               , PathSum.pathsumBuyQty pathSum
               , PathSum.pathsumSellQty pathSum
               )
             )

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

selectNewestRunAllLiquidity numeraire slippage =
    selectRunAllLiquidity runQ numeraire slippage
  where
    runQ = newestFinishedRun numeraire slippage

selectSpecificRunAllLiquidity runId =
    selectRunAllLiquidity runQ
  where
    runQ = do
        run <- all_ (runs liquidityDb)
        guard_ $ pk run ==. val_ runId
        pure run

selectRunAllLiquidity runQ numeraire slippage offsetM limitM otherMinPctM = fmap (aggregateOther . map mkLiquidityData) $
    runSelectReturningList $ select $ newestRunAllLiquidity runQ numeraire slippage offsetM limitM
  where
    -- aggregate all 'LiquidityData' whose 'ldQty' relative to the sum of 'ldQty' in percent
    --  is less than 'otherMinPctM' into a single 'LiquidityData' with a 'ldCurrency' equal to "Other"
    --  (which appears at the *end* of the list)
    aggregateOther :: [LiquidityData] -> [LiquidityData]
    aggregateOther ldLst =
        let totalQty = sum $ map ldQty ldLst
            relativeToInPercent number relativeTo =
                realToFrac (number * 100) / realToFrac relativeTo
            aggregate minPct = foldr (aggregate' minPct) ([], Nothing) ldLst
            aggregate' minPct ld (lst, otherM) =
                if ldQty ld `relativeToInPercent` totalQty >= minPct
                    then (ld : lst, otherM)
                    else let addToExistingQty otherLd = otherLd { ldQty = ldQty otherLd + ldQty ld }
                             newOtherLd = ld { ldCurrency = "Other" }
                         in (lst, Just $ maybe newOtherLd addToExistingQty otherM)
            appendOther (lst, Nothing) = lst
            appendOther (lst, Just ld) = lst ++ [ld]
        in maybe ldLst (appendOther . aggregate) otherMinPctM
    mkLiquidityData (run, currency, buy, sell) = LiquidityData
        { ldRun = run
        , ldRunId = pk run
        , ldCurrency = currency
        , ldQty = buy + sell
        , ldQtyBuy = buy
        , ldQtySell = sell
        }

newestRunAllLiquidity
    :: Q Pg.Postgres LiquidityDb
        (QNested (QNested (QNested s)))
        (Run.RunT (QExpr Pg.Postgres (QNested (QNested (QNested s)))))
    -> Currency
    -> Double
    -> Maybe Integer
    -> Maybe Integer
    -> Q Pg.Postgres LiquidityDb s
        ( Run.RunT (QGenExpr QValueContext Pg.Postgres s)
        , QGenExpr QValueContext Pg.Postgres s Text
        , QGenExpr QValueContext Pg.Postgres s PathSum.Int64
        , QGenExpr QValueContext Pg.Postgres s PathSum.Int64
        )
newestRunAllLiquidity runQ numeraire slippage offsetM limitM =
    limit_ (fromMaybe 50 limitM) $
        offset_ (fromMaybe 0 offsetM) $
            orderBy_ (\(_, _, buyQty, sellQty) -> desc_ (buyQty + sellQty))
                newestRunCalcs
  where
    newestRunCalcs = do
        run <- runQ
        calc <- finishedCryptoCalcsForRun run (Just numeraire) (Just slippage)
        pathSum <- sumForCalc calc
        pure ( run
             , getSymbol $ Calc.calculationCurrency calc
             , PathSum.pathsumBuyQty pathSum
             , PathSum.pathsumSellQty pathSum
             )

selectNewestFinishedRunId
    :: Currency
    -> Double
    -> Pg.Pg (Maybe Run.RunId)
selectNewestFinishedRunId numeraire slippage = do
    fmap pk <$> runSelectReturningOne (select $ newestFinishedRun numeraire slippage)

newestFinishedRun
    :: Currency
    -> Double
    -> Q Pg.Postgres LiquidityDb s (Run.RunT (QGenExpr QValueContext Pg.Postgres s))
newestFinishedRun numeraire slippage =
    limit_ 1 $
        orderBy_ (desc_ . Run.runId) $ do
            run <- all_ (runs liquidityDb)
            finishedRun run (Just numeraire) (Just slippage)
            pure run

selectNewestRunAllPaths
    :: Run.RunId
    -> Currency
    -> Double
    -> Maybe Word
    -> Pg.Pg (Maybe Query.Graph.GraphData)
selectNewestRunAllPaths runId numeraire slippage limitM = do
    cryptoQtys <- map (first toS) <$> runSelectReturningList (select $ newestRunCryptoQtys runId numeraire slippage)
    fmap (fmap (Query.Graph.toGraphData numeraire limitM cryptoQtys) . headMay . groupNestByFst) $
        runSelectReturningList $ select $ newestRunAllPaths runId numeraire slippage

newestRunCryptoQtys
    :: Run.RunId
    -> Currency
    -> Double
    -> Q Pg.Postgres LiquidityDb s
        ( QGenExpr QValueContext Pg.Postgres s Text
        , QGenExpr QValueContext Pg.Postgres s PathSum.Int64
        )
newestRunCryptoQtys runId numeraire slippage = do
    run <- theRun
    calc <- finishedCalcsForRun run (Just numeraire) (Just slippage)
    pathSum <- sumForCalc calc
    let qtySum = PathSum.pathsumBuyQty pathSum + PathSum.pathsumSellQty pathSum
    pure (getSymbol $ Calc.calculationCurrency calc, qtySum)
  where
    theRun = do
        run <- all_ (runs liquidityDb)
        guard_ $ pk run ==. val_ runId
        pure run

newestRunAllPaths
    :: Run.RunId
    -> Currency
    -> Double
    -> Q Pg.Postgres LiquidityDb s
        ( Run.RunT (QGenExpr QValueContext Pg.Postgres s)
        ,   ( QGenExpr QValueContext Pg.Postgres s Text
            , QGenExpr QValueContext Pg.Postgres s PathSum.Int64
            , Path.PathT (QExpr Pg.Postgres s)
            )
        )
newestRunAllPaths runId numeraire slippage = do
    run <- theRun
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

    theRun = do
        run <- all_ (runs liquidityDb)
        guard_ $ pk run ==. val_ runId
        pure run

instance Json.ToJSON LiquidityData where
    toJSON = Json.genericToJSON App.Util.prefixOptions
instance Json.FromJSON LiquidityData where
    parseJSON = Json.genericParseJSON App.Util.prefixOptions
