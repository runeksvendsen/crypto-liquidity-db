{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs #-}
module Query.Calculations
( startCalculation
, insertMissingCalculations
, resetStalledCalculations
, insertCalcParam
, selectAllCalculations
, selectStalledCalculations
, selectUnfinishedCalcCount
, unfinishedCalcCount
, Calc.CalculationT(..)
, Calc.Calculation
, calcNumeraire
, calcCrypto
, NominalDiffTime
, Int64
)
where

import Internal.Prelude ( Text, NominalDiffTime )
import App.Orphans ()
import App.Monad (DbTx, asTx)
import qualified App.Util
import Database
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run
import qualified Schema.Currency as Currency
import qualified Schema.RunCurrency as RC
import qualified Schema.Calculation as Calc
import qualified Schema.CalculationParameter as CalcParam

import Database.Beam
import Database.Beam.Backend (BeamSqlBackendSyntax, SqlNull, BeamSqlBackend)
import Database.Beam.Backend.SQL.SQL92
import qualified Database.Beam.Postgres.Full as Pg
import qualified Database.Beam.Postgres as Pg
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning(runUpdateReturningList), MonadBeamInsertReturning(runInsertReturningList))
import Data.Maybe (fromMaybe)
import Data.Int (Int64)


calcNumeraire :: Calc.Calculation -> Text
calcNumeraire = Currency.getSymbol . Calc.calculationNumeraire

calcCrypto :: Calc.Calculation -> Text
calcCrypto = Currency.getSymbol . Calc.calculationCurrency

startCalculation
    :: ( MonadBeam be m
        , BeamSqlBackendSyntax be ~ Pg.PgCommandSyntax
        , FromBackendRow be Calc.Int32
        , FromBackendRow be Calc.Int64
        , FromBackendRow be Text
        , FromBackendRow be Run.Int32
        , FromBackendRow be Double
        , FromBackendRow be Calc.UTCTime
        , FromBackendRow be SqlNull
        )
    => Calc.UTCTime
    -> m (Maybe Calc.Calculation)
startCalculation now = fmap castSingleResult .
    Pg.runPgUpdateReturningList $
    Pg.updateReturning (calculations liquidityDb)
        (\c -> Calc.calculationStartTime c <-. just_ (val_ now))
        (\c -> Calc.calculationId c ==. subquery_ (fst <$> selectQ))
        id
  where
    castSingleResult [] = Nothing
    castSingleResult [res] = Just res
    castSingleResult lst = error $ "BUG (startCalculation): selectQ did not return exactly one result: " ++ show lst
    getRunId (Run.RunId runId) = runId
    --  SELECT (run__id, id) FROM calculations WHERE start_time IS NULL ORDER BY (run__id, id) LIMIT 1 FOR UPDATE SKIP LOCKED
    selectQ =
        limit_ 1 $
        orderBy_ (\(calcId, runId) -> (asc_ runId, asc_ calcId)) $
        Pg.lockingFor_ Pg.PgSelectLockingStrengthUpdate (Just Pg.PgSelectLockingOptionsSkipLocked) $ do
            (calcLock, calculation) <- Pg.locked_ (calculations liquidityDb)
            guard_ $ isNothing_ (Calc.calculationStartTime calculation)
            pure $ (Calc.calculationId calculation, getRunId $ Calc.calculationRun calculation)
                `Pg.withLocks_` calcLock

-- | Set started calculations older than @timeout@ to unstarted
resetStalledCalculations :: NominalDiffTime -> DbTx ()
resetStalledCalculations timeout = asTx $ do
    timeoutTime <- calcExpirationTime timeout
    runUpdate $ update
        (calculations liquidityDb)
        (\calc' -> Calc.calculationStartTime calc' <-. nothing_)
        (isStalledCalculation timeoutTime)

calcExpirationTime timeout = do
    now <- liftIO App.Util.currentTime
    return $ (- timeout) `App.Util.addUTCTime` now

isStalledCalculation timeoutTime calc' = do
    isNothing_ (Calc.calculationDurationSeconds calc') &&.
            Calc.calculationStartTime calc' <. just_ (val_ timeoutTime)

selectStalledCalculations timeout = do
    timeoutTime <- calcExpirationTime timeout
    runSelectReturningList $ select $ stalledCalculations timeoutTime

selectUnfinishedCalcCount
    :: ( MonadBeam be m
       , HasQBuilder be
       , FromBackendRow be Int64
       , HasSqlEqualityCheck be Double
       , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) (Maybe Double)
       )
       => m Int64
selectUnfinishedCalcCount =
    fmap (fromMaybe (error "BUG: selectUnfinishedCalcCount: COUNT returned Nothing")) $ do
        runSelectReturningOne $ select $
            unfinishedCalcCount (all_ (calculations liquidityDb))

unfinishedCalcCount calcQ =
    aggregate_ (const $ as_ @Int64 countAll_) $ do
        calc <- calcQ
        guard_ $ Calc.calculationDurationSeconds calc ==. val_ Nothing
        pure calc

stalledCalculations timeoutTime = do
    calc' <- all_ (calculations liquidityDb)
    guard_ $ isStalledCalculation timeoutTime calc'
    pure calc'

selectAllCalculations
    :: ( MonadBeam be m
       , FromBackendRow be Calc.Int32
       , FromBackendRow be Calc.Int64
       , FromBackendRow be Text
       , FromBackendRow be Double
       , FromBackendRow be Calc.UTCTime
       , FromBackendRow be SqlNull
       , HasQBuilder be
       )
    => m [Calc.Calculation]
selectAllCalculations = do
    runSelectReturningList $ select $ all_ (calculations liquidityDb)

insertMissingCalculations :: Calc.UTCTime -> DbTx [Calc.Calculation]
insertMissingCalculations now = asTx $ do
    rcCalcParam <- selectMissingCalculations
    runInsertReturningList $
        insert (calculations liquidityDb) $
        insertExpressions $ map (uncurry $ Calc.new now) rcCalcParam

selectMissingCalculations ::
    ( MonadBeam be m
    , BeamSqlBackend be
    , HasQBuilder be
    , FromBackendRow be Run.Int32
    , FromBackendRow be Text
    , FromBackendRow be Double
    , HasSqlEqualityCheck be Run.Int32
    , HasSqlEqualityCheck be Text
    , HasSqlEqualityCheck be Double
    )
    => m [(RC.RunCurrency, CalcParam.CalcParam)]
selectMissingCalculations =
    runSelectReturningList $ select runCurrencyWithNoCalculation

runCurrencyWithNoCalculation ::
    ( HasQBuilder be
    , HasSqlEqualityCheck be Run.Int32
    , HasSqlEqualityCheck be Text
    , HasSqlEqualityCheck be Double
    )
    => Q be LiquidityDb s
        ( RC.RunCurrencyT (QExpr be s)
        , CalcParam.CalcParamT (QExpr be s)
        )
runCurrencyWithNoCalculation = do
    rc  <- all_ $ run_currencys liquidityDb
    calcParam  <- all_ $ calculation_parameters liquidityDb
    calculation <- leftJoin_ (all_ $ calculations liquidityDb)
        (\calc -> Calc.calculationRun calc ==. RC.rcRun rc &&.
            Calc.calculationCurrency calc ==. RC.rcCurrency rc &&.
            Calc.calculationNumeraire calc ==. CalcParam.cpNumeraire calcParam &&.
            Calc.calculationSlippage calc ==. CalcParam.cpSlippage calcParam
        )
    guard_ $
        isNothing_ calculation &&.
        RC.rcCurrency rc /=. CalcParam.cpNumeraire calcParam -- don't calculate the liquidity for the numeraire currency
    pure (rc, calcParam)

insertCalcParam :: Text -> Double -> DbTx ()
insertCalcParam numeraire slippage = asTx $ do
    runInsert $
        insert (calculation_parameters liquidityDb) $
            insertExpressions
                [ CalcParam.CalcParam (Currency.CurrencyId $ val_ numeraire) (val_ slippage)
                ]
