{-# LANGUAGE GADTs #-}
module Query.Calculations
( startCalculation
, insertMissingCalculations
, selectUnstartedCalculations
, selectUnfinishedCalculations
, insertCalcParam
)
where

import App.Orphans ()
import Database
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run
import qualified Schema.Currency as Currency
import qualified Schema.RunCurrency as RC
import qualified Schema.Calculation as Calc
import qualified Schema.CalculationParameter as CalcParam
import qualified Query.Currencies as QC

import Database.Beam
import Database.Beam.Backend (BeamSqlBackendSyntax, SqlNull, BeamSqlBackend)
import qualified Database.Beam.Postgres.Full as Pg
import qualified Database.Beam.Postgres as Pg
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning(runInsertReturningList))
import Data.Maybe (fromMaybe)


startCalculation
  :: ( MonadBeam be m
     , BeamSqlBackendSyntax be ~ Pg.PgCommandSyntax
     , FromBackendRow be Calc.Int32
     , FromBackendRow be Run.Word32
     , FromBackendRow be Double
     , FromBackendRow be Float
     , FromBackendRow be Calc.LocalTime
     , FromBackendRow be SqlNull
     )
  => m (Maybe Calc.Calculation)
startCalculation = fmap castSingleResult .
    Pg.runPgUpdateReturningList $
    Pg.updateReturning (calculations liquidityDb)
        (\c -> Calc.calculationStartTime c <-. just_ currentTimestamp_)
        (\c -> Calc.calculationId c ==. subquery_ selectQ)
        id
  where
    castSingleResult [] = Nothing
    castSingleResult [res] = Just res
    castSingleResult lst = error $ "startCalculation: selectQ did not return exactly one result: " ++ show lst
    --  SELECT id FROM calculations WHERE start_time IS NULL LIMIT 1 FOR UPDATE SKIP LOCKED
    selectQ =
        limit_ 1 $
        Pg.lockingFor_ Pg.PgSelectLockingStrengthUpdate (Just Pg.PgSelectLockingOptionsSkipLocked) $ do
            (calcLock, calculation) <- Pg.locked_ (calculations liquidityDb)
            guard_ $ isNothing_ (Calc.calculationStartTime calculation)
            pure (Calc.calculationId calculation `Pg.withLocks_` calcLock)

-- | Calculations not yet started
selectUnstartedCalculations ::
    ( MonadBeam be m
    , BeamSqlBackend be
    , HasQBuilder be
    , FromBackendRow be Run.Word32
    , FromBackendRow be Currency.Int32
    , FromBackendRow be Float
    , FromBackendRow be Double
    , FromBackendRow be Calc.LocalTime
    , FromBackendRow be SqlNull
    ) => m [Calc.Calculation]
selectUnstartedCalculations = do
    runSelectReturningList $ select unstartedCalculation
  where
    unstartedCalculation = do
        calculation <- all_ $ calculations liquidityDb
        guard_ $ isNothing_ (Calc.calculationStartTime calculation)
        pure calculation

-- | Calculations started but not yet finished
selectUnfinishedCalculations ::
    ( MonadBeam be m
    , BeamSqlBackend be
    , HasQBuilder be
    , FromBackendRow be Run.Word32
    , FromBackendRow be Currency.Int32
    , FromBackendRow be Float
    , FromBackendRow be Double
    , FromBackendRow be Calc.LocalTime
    , FromBackendRow be SqlNull
    ) => m [Calc.Calculation]
selectUnfinishedCalculations = do
    runSelectReturningList $ select unstartedCalculation
  where
    unstartedCalculation = do
        calculation <- all_ $ calculations liquidityDb
        guard_ $ isJust_ (Calc.calculationStartTime calculation) &&.
            isNothing_ (Calc.calculationDurationSeconds calculation)
        pure calculation

insertMissingCalculations :: Pg.Pg [Calc.Calculation]
insertMissingCalculations = do
    rcCalcParam <- selectMissingCalculations
    runInsertReturningList $
        insert (calculations liquidityDb) $
        insertExpressions $ map (uncurry Calc.new) rcCalcParam

selectMissingCalculations ::
    ( MonadBeam be m
    , BeamSqlBackend be
    , HasQBuilder be
    , FromBackendRow be Run.Word32
    , FromBackendRow be Currency.Int32
    , FromBackendRow be Double
    , HasSqlEqualityCheck be Run.Word32
    , HasSqlEqualityCheck be Currency.Int32
    , HasSqlEqualityCheck be Double
    )
    => m [(RC.RunCurrency, CalcParam.CalcParam)]
selectMissingCalculations =
    runSelectReturningList $ select runCurrencyWithNoCalculation

runCurrencyWithNoCalculation ::
    ( HasQBuilder be
    , HasSqlEqualityCheck be Run.Word32
    , HasSqlEqualityCheck be Currency.Int32
    , HasSqlEqualityCheck be Double
    )
    => Q be LiquidityDb s
        ( RC.RunCurrencyT (QExpr be s)
        , CalcParam.CalcParamT (QExpr be s)
        )
runCurrencyWithNoCalculation = do
    rc  <- all_ $ runCurrencies liquidityDb
    calcParam  <- all_ $ calculationParameters liquidityDb
    calculation <- leftJoin_ (all_ $ calculations liquidityDb)
        (\calc -> Calc.calculationRun calc ==. RC.runCurrencyRun rc &&.
            Calc.calculationCurrency calc ==. RC.runCurrencyCurrency rc &&.
            Calc.calculationNumeraire calc ==. CalcParam.calcParamNumeraire calcParam &&.
            Calc.calculationSlippage calc ==. CalcParam.calcParamSlippage calcParam
        )
    guard_ (isNothing_ calculation)
    pure (rc, calcParam)

insertCalcParam numeraire slippage = do
    numeraireId <- fromMaybe (error $ "Numeraire " ++ show numeraire ++ " not found")
        <$> runSelectReturningOne (select $ QC.lookupCurrency numeraire)
    runInsert $
        insert (calculationParameters liquidityDb) $
            insertExpressions
                [ CalcParam.CalcParam (val_ numeraireId) (val_ slippage)
                ]
