{-# LANGUAGE RecordWildCards #-}
module Query.Books
( runBooks
, G.OrderBook
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


runBooks :: Run.RunId -> Pg.Pg [G.OrderBook Double]
runBooks runPk =
    map convertBook <$> Books.runBooks runPk

convertBook :: Books.OB -> G.OrderBook Double
convertBook Books.OB{..} =
    G.mkOrderBook
        (Vec.map convertOrder obBuyOrders)
        (Vec.map convertOrder obSellOrders)
        obVenue
        (toS obBase)
        (toS obQuote)
  where
    convertOrder (qty, price) = G.mkOrder qty price

-- subquery _ = aggregate_
--     (\(bookId, order) ->
--         ( group_ bookId
--         , group_ $ Order.orderIsBuy order
--         , if_ [ Order.orderIsBuy order `then_` max_ (Order.orderPrice order) ] (else_ $ min_ (Order.orderPrice order))
--         )  -- if_ [ cond1 `then_` result1, cond2 `then_` result2, ... ] (else_ elseResult)
--                                     -- CASE WHEN o.is_buy = true THEN MAX(o.price) ELSE MIN(o.price) END
--     ) $ do
--         book <- all_ (books liquidityDb)
--         order <- all_ (orders liquidityDb)
--         guard_ $ Order.orderBook order `references_` book
--         pure (Book.bookId book, order)


{-
    SUBQUERY =
        SELECT
            b.id,
            o.is_buy,
            (CASE WHEN o.is_buy = true THEN MAX(o.price) ELSE MIN(o.price) END) as best_price
        FROM
            books b JOIN orders o ON b.id = o.book__id
        GROUP BY
            b.id, o.is_buy
        ORDER BY
            b.id

    SELECT
        b.base,
        b.quote,
        b.venue,
        o.is_buy,
        SUM(o.price * o.qty) as quote_sum
    FROM
        books b JOIN orders o ON b.id = o.book__id
                JOIN SUBQUERY as sub ON sub.id = b.id
    WHERE
        b.run__id = 1 AND
        ABS(1 - o.price / sub.best_price) <= 0.005 -- VARIABLE
    GROUP BY
        b.run__id,
        b.id,
        b.base,
        b.quote,
        b.venue,
        o.is_buy
    ORDER BY
        SUM(o.price * o.qty) DESC, b.id, is_buy;
-}
