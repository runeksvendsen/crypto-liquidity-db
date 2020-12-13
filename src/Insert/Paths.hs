{-# LANGUAGE GADTs #-}
module Insert.Paths where

import qualified Schema.Path as Path
import qualified Schema.PathPart as PP
import qualified Schema.PathQty as PQty

import qualified OrderBook.Graph as G

import qualified Data.List.NonEmpty as NE
import Database.Beam
import Schema.Venue (VenueId)
import Schema.Currency (CurrencyId)


-- Path
--     { pathId
--     , pathStart
--     }

-- PathPart
--     { pathPartPath
--     , pathPartIndex
--     , pathPartVenue
--     , pathPartCurrency
--     }

-- PathQty
--     { pathQtyRun
--     , pathQtyPath
--     , pathQtyQty
--     , pathQtyPriceLow
--     , pathQtyPriceHigh
--     }


toPath path =
    Path.Path default_ (val_ $ G.pStart descr)
  where
    descr = G.pathDescr path
    mkPathParts pathPk = NE.zipWith (fromWithIndex pathPk) (NE.fromList [0..]) (G.pMoves descr)
    fromWithIndex :: Path.PathId -> Int -> (VenueId, CurrencyId) -> PP.PathPartT f
    fromWithIndex pathPk idx (venue, currency) = PP.PathPart
        { PP.pathPartPath = val_ pathPk
        , PP.pathPartIndex = val_ idx
        , PP.pathPartVenue = val_ venue
        , PP.pathPartCurrency = val_ currency
        }



insert :: ([G.SellPath], [G.BuyPath]) -> m ()
insert (sellPath, buyPath) =
    undefined
--   where
