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
    convertOrder (price, qty) = G.mkOrder qty price
