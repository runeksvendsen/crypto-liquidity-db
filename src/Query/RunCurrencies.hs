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


import Database.Beam
import Database.Beam.Backend (BeamSqlBackendSyntax, Sql92SelectSyntax, Sql92SelectSelectTableSyntax, Sql92SelectTableExpressionSyntax, Sql92ExpressionValueSyntax, HasSqlValueSyntax, BeamSqlBackend)
import Data.Text (Text)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning(runInsertReturningList))
import Schema.Currency (Int32)


-- TODO: Tx
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
       )
       => m [RC.RunCurrency]
insertMissingRunCurrencies = do
    missingCurrencies <- selectMissingRunCurrencies
    fmap concat $ mapM runInsertReturningList $ for missingCurrencies $ \(runId, currencys') ->
        insert (run_currencys liquidityDb) $
        insertValues $ for currencys' $ \currency ->
            RC.RunCurrency runId (Currency.CurrencyId currency)

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

missingRunCurrencies
    :: ( HasQBuilder be
       , HasSqlEqualityCheck be Run.Word32
       )
    => Q be LiquidityDb s
        ( PrimaryKey Run.RunT (QExpr be s)
        , (QGenExpr QValueContext be s Text
        , QGenExpr QValueContext be s Text)
        )
missingRunCurrencies = do
    run <- runsWithNoCurrencies
    baseQuote <- runBaseQuote run
    pure (pk run, baseQuote)

runsWithNoCurrencies
    :: ( HasQBuilder be
       , HasSqlEqualityCheck be Run.Word32
       )
    => Q be LiquidityDb s (Run.RunT (QExpr be s))
runsWithNoCurrencies = do
    run  <- all_ $ runs liquidityDb
    guard_ $ not_ $ exists_ $
        filter_
            (\rc -> RC.rcRun rc `references_` run)
            (all_ $ run_currencys liquidityDb)
    pure run

runBaseQuote
    :: HasSqlEqualityCheck be Run.Word32
    => Run.RunT (QGenExpr QValueContext be s)
    -> Q be LiquidityDb s
        ( QGenExpr QValueContext be s Text
          , QGenExpr QValueContext be s Text
        )
runBaseQuote run = nub_ $ do
    book <- all_ $ books liquidityDb
    guard_ $ Book.bookRun book `references_` run
    pure (Book.bookBase book, Book.bookQuote book)

runCurriencies :: [(Run.RunId, (Text, Text))] -> [(Run.RunId, [Text])]
runCurriencies runBaseQuoteL = do
    map (\lst -> (fst $ head lst, uniqueCurrencies $ map snd lst)) $ groupOn fst runBaseQuoteL
  where
    uniqueCurrencies = uniqueOn id . uncurry (++) . unzip
    uniqueOn f = map head . groupOn f
