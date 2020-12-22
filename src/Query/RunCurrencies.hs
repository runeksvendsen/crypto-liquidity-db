{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Query.RunCurrencies
( insertMissingRunCurrencies
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
import Database.Beam.Backend (BeamSqlBackendSyntax, Sql92SelectSyntax, Sql92SelectSelectTableSyntax, Sql92SelectTableExpressionSyntax, Sql92ExpressionValueSyntax, HasSqlValueSyntax, BeamSqlBackend)
import Data.Text (Text)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning(runInsertReturningList))
import Schema.Currency (Int32)


-- TODO: Tx for each run
insertMissingRunCurrencies
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
       => m [(Run.RunId, [Text])]
insertMissingRunCurrencies = do
    missingCurrencies <- selectMissingRunCurrencies
    -- currencys
    QC.insertMissingCurrencies (concatMap snd missingCurrencies)
    -- venues
    runSelectReturningList (select allVenuesUnique) >>= QV.insertMissingVenues
    -- run_currencys
    mapM_ runInsert $ for missingCurrencies $ \(runId, currencys') ->
        insert (run_currencys liquidityDb) $
        insertValues $ for currencys' $ \currency ->
            RC.RunCurrency runId (Currency.CurrencyId currency)
    return missingCurrencies
  where
    allVenuesUnique = nub_ $ do
        book <- all_ $ books liquidityDb
        pure $ Book.bookVenue book

selectMissingRunCurrencies
    :: ( MonadBeam be m
       , BeamSqlBackend be
       , HasQBuilder be
       , FromBackendRow be Run.Word32
       , FromBackendRow be Text
       , HasSqlEqualityCheck be Run.Word32
       )
    => m [(Run.RunId, [Text])]
selectMissingRunCurrencies = do
    runCurriencies <$> runSelectReturningList (select missingRunCurrencies)
  where
    runCurriencies runBaseQuoteL =
        let uniqueCurrencies = uniqueOn id . uncurry (++) . unzip
        in map (\lst -> (fst $ head lst, uniqueCurrencies $ map snd lst)) $ groupOn fst runBaseQuoteL

missingRunCurrencies
    :: ( HasQBuilder be
       , HasSqlEqualityCheck be Run.Word32
       )
    => Q be LiquidityDb s
        ( PrimaryKey Run.RunT (QExpr be s)
        , (QGenExpr QValueContext be s Text
        , QGenExpr QValueContext be s Text)
        )
missingRunCurrencies = nub_ $ do
    runPk <- runsWithNoCurrencies
    baseQuote <- runBaseQuote runPk
    pure (runPk, baseQuote)
  where
    runsWithNoCurrencies = do
        run  <- all_ $ runs liquidityDb
        guard_ $ not_ $ exists_ $
            filter_
                (\rc -> RC.rcRun rc `references_` run)
                (all_ $ run_currencys liquidityDb)
        pure (pk run)
    runBaseQuote runId = do
        book <- all_ $ books liquidityDb
        guard_ $ Book.bookRun book ==. runId
        pure (Book.bookBase book, Book.bookQuote book)
