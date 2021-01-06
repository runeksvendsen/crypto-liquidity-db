{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Query.RunCurrencies
( insertRunRunCurrencies
)
where

import Internal.Prelude
import Database
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run
import qualified CryptoDepth.OrderBook.Db.Schema.Book as Book
import qualified Schema.RunCurrency as RC
import qualified Schema.Currency as Currency
import qualified Insert.Currencies as QC
import qualified Insert.Venues as QV

import Database.Beam
import Database.Beam.Backend (SqlSerial(unSerial),  BeamSqlBackendSyntax, Sql92SelectSyntax, Sql92SelectSelectTableSyntax, Sql92SelectTableExpressionSyntax, Sql92ExpressionValueSyntax, HasSqlValueSyntax, BeamSqlBackend)
import Data.Text (Text)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning(runInsertReturningList))
import Schema.Currency (Int32)
import Data.Maybe (listToMaybe)


-- TODO: Tx for each run
insertRunRunCurrencies
    :: ( MonadBeamInsertReturning be m
       , HasQBuilder be
       , FromBackendRow be Int32
       , FromBackendRow be Text
       , FromBackendRow be Book.Word32
       , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Text
       , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Book.Word32
       , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Int32
       , HasSqlEqualityCheck be Text
       , HasSqlEqualityCheck be Book.Word32
       , FromBackendRow be Book.UTCTime
       , MonadIO m
       )
       => m (Maybe (Run.Word32, [Text]))
insertRunRunCurrencies = do
    missingCurrenciesM <- selectRunWithoutRunCurrencies
    forM missingCurrenciesM $ \missingCurrencies@(runId, currencys') -> do
        -- currencys
        QC.insertMissingCurrencies (snd missingCurrencies)
        -- venues
        runSelectReturningList (select allVenuesUnique) >>= QV.insertMissingVenues
        -- run_currencys
        runInsert $
            insert (run_currencys liquidityDb) $
                insertValues $ for currencys' $ \currency ->
                    RC.RunCurrency (Run.RunId runId) (Currency.CurrencyId currency)
        return (unSerial runId, currencys')
  where
    allVenuesUnique = nub_ $ do
        book <- all_ $ books liquidityDb
        pure $ Book.bookVenue book

selectRunWithoutRunCurrencies
    :: ( MonadBeam be m
       , BeamSqlBackend be
       , HasQBuilder be
       , FromBackendRow be Run.Word32
       , FromBackendRow be Text
       , HasSqlEqualityCheck be Run.Word32
       )
    => m (Maybe (SqlSerial Run.Word32, [Text]))
selectRunWithoutRunCurrencies = do
    listToMaybe . runCurriencies <$> runSelectReturningList (select runWithoutRunCurrencies)
  where
    runCurriencies runBaseQuoteL =
        let uniqueCurrencies = uniqueOn id . uncurry (++) . unzip
        in map (\lst -> (fst $ head lst, uniqueCurrencies $ map snd lst)) $ groupOn fst runBaseQuoteL

runWithoutRunCurrencies
    :: ( HasQBuilder be
       , HasSqlEqualityCheck be Run.Word32
       )
    => Q be LiquidityDb s
        ( QGenExpr QValueContext be s (SqlSerial Run.Word32)
        , (QGenExpr QValueContext be s Text
        , QGenExpr QValueContext be s Text)
        )
runWithoutRunCurrencies = nub_ $ do
    runId <- firstRunWithNoCurrencies undefined
    baseQuote <- runBaseQuote runId
    pure (runId, baseQuote)
  where
    runWithNoCurrencies = do
        run <- all_ (runs liquidityDb)
        guard_ $ not_ $ exists_ $
            filter_
                (\rc -> RC.rcRun rc `references_` run)
                (all_ $ run_currencys liquidityDb)
        pure (Run.runId run)
    runBaseQuote runId = do
        book <- all_ $ books liquidityDb
        guard_ $ Book.bookRun book ==. Run.RunId runId
        pure (Book.bookBase book, Book.bookQuote book)
    firstRunWithNoCurrencies _ =
            limit_ 1 $
            orderBy_ asc_
            runWithNoCurrencies
