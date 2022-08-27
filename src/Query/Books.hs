{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Query.Books
( runBooks
, runBook
, G.OrderBook
, BookResult(..)
, emptyBookResult
  -- * Re-exports
, G.baseQuote
, G.bookVenue
)
where

import Internal.Prelude
import Database

import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run
import qualified CryptoDepth.OrderBook.Db.Schema.Book as Book
import qualified CryptoDepth.OrderBook.Db.Schema.Order as Order
import qualified CryptoDepth.OrderBook.Db.Query as Books
import qualified OrderBook.Graph.Types as G

import Database.Beam
import qualified Database.Beam.Postgres as Pg
import qualified Data.Vector as Vec
import qualified Data.Text as T
import Data.Maybe (fromMaybe, maybeToList)


runBooks :: Run.RunId -> Pg.Pg [G.OrderBook Double]
runBooks runPk =
    map convertBook <$> Books.runBooks runPk

-- | See docs for 'CryptoDepth.OrderBook.Db.Query.runBook'
runBook
    :: Run.RunId
    -> Text -- ^ Venue
    -> G.Currency -- ^ currency1: either base or quote
    -> G.Currency -- ^ currency2: @if currency1 is base then quote else base@
    -> Pg.Pg (BookResult (G.OrderBook Double))
runBook runPk venue bookCurrency1 bookCurrency2 =
    toResult . map convertBook <$> Books.runBook venue bookCurrency1' bookCurrency2' runPk
  where
    bookCurrency1' = toS bookCurrency1
    bookCurrency2' = toS bookCurrency2

    toResult :: [G.OrderBook Double] -> BookResult (G.OrderBook Double)
    toResult [] = emptyBookResult
    toResult [ob] = BookResult (Just ob) (maybeToList $ checkCrossed ob) []
    toResult obs@(ob : _) = -- TODO: join multiple orderbooks
        let warning = T.unwords
                [ "Found multiple orderbooks for "
                , T.intercalate "/" [venue, bookCurrency1', bookCurrency2'] <> ":"
                , T.intercalate ", " (map showBook obs)
                ]
        in BookResult (Just ob) (maybeToList $ checkCrossed ob) [warning]

    checkCrossed :: G.OrderBook Double -> Maybe Text
    checkCrossed ob =
        let ob' = G.sortOrders ob
            bids' = G.bookBids ob'
            asks' = G.bookAsks ob'
        in do
            highestBid <- G.orderPrice <$> Vec.headM bids'
            lowestAsk <- G.orderPrice <$> Vec.headM asks'
            if highestBid >= lowestAsk
                then pure $ T.unwords
                    [ "Orderbook"
                    , showBook ob
                    , "crossed."
                    , "Highest bid:"
                    , T.pack $ show highestBid
                    , "Lowest ask:"
                    , T.pack $ show lowestAsk
                    ]
                else Nothing

    showBook ob' =
        let (base, quote) = G.baseQuote ob'
        in T.intercalate "/" [G.bookVenue ob', base, quote]

convertBook :: Books.OB -> G.OrderBook Double
convertBook Books.OB{..} =
    G.mkOrderBook
        (Vec.map convertOrder obBuyOrders)
        (Vec.map convertOrder obSellOrders)
        obVenue
        (toS obBase)
        (toS obQuote)
  where
    convertOrder (price, qty) = G.mkOrder qty price

-- |
data BookResult a = BookResult
    { result :: Maybe a
    , info :: [Text] -- ^ Possible exchange/venue bug. E.g. is the orderbook crossed?
    , warnings :: [Text] -- ^ Possible backend bug. E.g. did we find multiple orderbooks?
    } deriving (Eq, Show, Generic)

emptyBookResult :: BookResult a
emptyBookResult = BookResult Nothing [] []
