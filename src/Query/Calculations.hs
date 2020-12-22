{-# LANGUAGE GADTs #-}
module Query.Calculations
( startCalculation
, insertMissingCalculations
, selectUnstartedCalculations
, selectUnfinishedCalculations
, insertCalcParam
, Calculation(..)
)
where

import App.Orphans ()
import Database
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run
import qualified Schema.Currency as Currency
import qualified Schema.RunCurrency as RC
import qualified Schema.Calculation as Calc
import qualified Schema.CalculationParameter as CalcParam

import Database.Beam
import Database.Beam.Backend (BeamSqlBackendSyntax, SqlNull, BeamSqlBackend)
import qualified Database.Beam.Postgres.Full as Pg
import qualified Database.Beam.Postgres as Pg
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning(runInsertReturningList))
import Data.Maybe (fromMaybe)
import Internal.Prelude (Text)


data Calculation = Calculation
    { calcCalc :: Calc.Calculation
    , calcNumeraire :: Text
    , calcCrypto :: Text
    }


startCalculation :: Calc.LocalTime -> Pg.Pg (Maybe Calculation)
startCalculation now = do
    calcM <- startCalculation' now
    case calcM of
        Nothing -> return Nothing
        Just calc -> do
            (numeraire, crypto) <- fromMaybe (error "") <$> runSelectReturningOne (select $ getSymbols calc)
            return $ Just $ Calculation calc numeraire crypto
  where
    getSymbols calc = do
        numeraire <- all_ (currencys liquidityDb)
        crypto <- all_ (currencys liquidityDb)
        guard_ $ val_ (Calc.calculationNumeraire calc) ==. pk numeraire
        guard_ $ val_ (Calc.calculationCurrency calc) ==. pk crypto
        return (Currency.currencySymbol numeraire, Currency.currencySymbol crypto)


startCalculation'
    :: ( MonadBeam be m
        , BeamSqlBackendSyntax be ~ Pg.PgCommandSyntax
        , FromBackendRow be Calc.Int32
        , FromBackendRow be Text
        , FromBackendRow be Run.Word32
        , FromBackendRow be Double
        , FromBackendRow be Calc.LocalTime
        , FromBackendRow be SqlNull
        )
    => Calc.LocalTime
    -> m (Maybe Calc.Calculation)
startCalculation' now = fmap castSingleResult .
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

-- | Calculations not yet started
selectUnstartedCalculations ::
    ( MonadBeam be m
    , BeamSqlBackend be
    , HasQBuilder be
    , FromBackendRow be Run.Word32
    , FromBackendRow be Calc.Int32
    , FromBackendRow be Text
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
    , FromBackendRow be Calc.Int32
    , FromBackendRow be Text
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

insertMissingCalculations :: Calc.LocalTime -> Pg.Pg ()
insertMissingCalculations now = do
    rcCalcParam <- selectMissingCalculations
    runInsert $
        insert (calculations liquidityDb) $
        insertExpressions $ map (uncurry $ Calc.new now) rcCalcParam

selectMissingCalculations ::
    ( MonadBeam be m
    , BeamSqlBackend be
    , HasQBuilder be
    , FromBackendRow be Run.Word32
    , FromBackendRow be Text
    , FromBackendRow be Double
    , HasSqlEqualityCheck be Run.Word32
    , HasSqlEqualityCheck be Text
    , HasSqlEqualityCheck be Double
    )
    => m [(RC.RunCurrency, CalcParam.CalcParam)]
selectMissingCalculations =
    runSelectReturningList $ select runCurrencyWithNoCalculation

runCurrencyWithNoCalculation ::
    ( HasQBuilder be
    , HasSqlEqualityCheck be Run.Word32
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

insertCalcParam :: Text -> Double -> Pg.Pg ()
insertCalcParam numeraire slippage = do
    runInsert $
        insert (calculation_parameters liquidityDb) $
            insertExpressions
                [ CalcParam.CalcParam (Currency.CurrencyId $ val_ numeraire) (val_ slippage)
                ]
