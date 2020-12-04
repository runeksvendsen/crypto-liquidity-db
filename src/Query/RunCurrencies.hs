module Query.RunCurrencies
( selectMissingRunCurrencies
)
where

import Database
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run
import qualified CryptoDepth.OrderBook.Db.Schema.Book as Book
import qualified Schema.RunCurrency as RC

import Database.Beam
import Database.Beam.Backend (SqlSerial, BeamSqlBackend)
import Data.Text (Text)
import Data.List (sortOn, groupBy)


selectMissingRunCurrencies
    :: ( MonadBeam be m
       , BeamSqlBackend be
       , HasQBuilder be
       , FromBackendRow be Run.Word32
       , FromBackendRow be Text
       , HasSqlEqualityCheck be Run.Word32
       )
    => m [(SqlSerial Run.Word32, [Text])]
selectMissingRunCurrencies = do
    runCurriencies <$> runSelectReturningList (select missingRunCurrencies)

missingRunCurrencies
    :: ( HasQBuilder be
       , HasSqlEqualityCheck be Run.Word32
       )
    => Q be LiquidityDb s
        ( QGenExpr QValueContext be s (SqlSerial Run.Word32)
        , (QGenExpr QValueContext be s Text
        , QGenExpr QValueContext be s Text)
        )
missingRunCurrencies = do
    run <- runsWithNoCurrencies
    baseQuote <- runBaseQuote run
    pure (Run.runId run, baseQuote)

runsWithNoCurrencies
    :: ( HasQBuilder be
       , HasSqlEqualityCheck be Run.Word32
       )
    => Q be LiquidityDb s (Run.RunT (QExpr be s))
runsWithNoCurrencies = do
    run  <- all_ $ runs liquidityDb
    guard_ $ not_ $ exists_ $
        filter_
            (\rc -> RC.runCurrencyRun rc `references_` run)
            (all_ $ runCurrencies liquidityDb)
    pure run

runBaseQuote
    :: HasSqlEqualityCheck be Run.Word32
    => Run.RunT (QGenExpr QValueContext be s)
    -> Q be LiquidityDb s
        ( QGenExpr QValueContext be s Text
          , QGenExpr QValueContext be s Text
        )
runBaseQuote run = do
    book <- all_ $ books liquidityDb
    guard_ $ Book.bookRun book `references_` run
    pure (Book.bookBase book, Book.bookQuote book)

runCurriencies :: [(SqlSerial Run.Word32, (Text, Text))] -> [(SqlSerial Run.Word32, [Text])]
runCurriencies runBaseQuoteL = do
    map (\lst -> (fst $ head lst, uniqueCurrencies $ map snd lst)) $ groupOn fst runBaseQuoteL
  where
    uniqueCurrencies = uniqueOn id . uncurry (++) . unzip
    uniqueOn f = map head . groupOn f
    groupOn f = groupBy (\a1 a2 -> f a1 == f a2) . sortOn f
