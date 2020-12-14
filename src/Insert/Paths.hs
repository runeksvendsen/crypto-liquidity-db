{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs #-}

module Insert.Paths
( insertAllPathQtys
)
where

import           Data.List                                ( sort )
import qualified Data.List.NonEmpty                       as NE

import           Database
import           Database.Beam

import           Internal.Prelude

import qualified OrderBook.Graph                          as G

import           Schema.Currency
                 ( PrimaryKey(CurrencyId) )
import qualified Schema.Path                              as Path
import qualified Schema.PathPart                          as PP
import qualified Schema.PathQty                           as PQty
import           Schema.Venue
                 ( PrimaryKey(VenueId) )


insertAllPathQtys calcPk pathList =
    let groupedPaths = groupOn G.pathDescr pathList
        pathPriceQtyLst = map (\lst ->
                   (G.pathDescr $ head lst, (map G.pQty lst, map G.pPrice lst)))
                  groupedPaths
    in
        forM_ pathPriceQtyLst $ \(pathDescr, (pathQtyLst, pathPrices)) ->
        insertSinglePathQty calcPk
                         pathDescr
                         (round $ sum pathQtyLst)
                         (sort pathPrices)

insertSinglePathQty calcPk pathDescr pathQty sortedPathPrices = do
    pathT <- pathLookupOrInsert pathDescr
    runInsert $ pathQtyInsert (pk pathT)
  where
    pathQtyInsert pathPk = insert (pathQtys liquidityDb) $
        insertValues [ PQty.PathQty { PQty.pathQtyCalc      = calcPk
                                    , PQty.pathQtyPath      = pathPk
                                    , PQty.pathQtyQty       = pathQty
                                    , PQty.pathQtyPriceLow  = head sortedPathPrices
                                    , PQty.pathQtyPriceHigh = last sortedPathPrices
                                    }
                     ]

pathLookupOrInsert pathDescr = do
    let pathParts' = mkPathParts (Path.PathId 0) -- we use a dummy path ID (which is ignored by "pathLookup")
    pathTM <- runSelectReturningOne $ select $ pathLookup pathParts'
    case pathTM of
        Just pathT -> return pathT
        Nothing -> do
            pathT <- runInsertReturningOne pathInsert
            runInsert $ pathPartInsert (pk pathT)
            return pathT
  where
    pathEdges = G.pMoves pathDescr
    pathInsert = insert (paths liquidityDb) $
        insertExpressions [ Path.Path default_
                                      (val_ . fromIntegral $ length pathEdges)
                                      (CurrencyId (val_ . toS $
                                                   G.pStart pathDescr))
                          ]
    pathLookup pathPartLst = do
        path <- all_ (paths liquidityDb)
        guard_ $ Path.pathLength path ==. val_ (fromIntegral $ length pathPartLst)
        pathPart <- partsForPath path
        guard_ $ foldr
            (\pathPart' state ->
                state ||.
                PP.pathPartIndex pathPart ==. val_ (PP.pathPartIndex pathPart') ||.
                PP.pathPartVenue pathPart ==. val_ (PP.pathPartVenue pathPart') ||.
                PP.pathPartCurrency pathPart ==. val_ (PP.pathPartCurrency pathPart')
            )
            (val_ True)
            pathPartLst
        pure path
    pathPartInsert =
        insert (pathParts liquidityDb) . insertValues . mkPathParts
    mkPathParts pathPk =
        for (zip [ 0 .. ] (NE.toList pathEdges)) $ \(idx, (venue, currency)) ->
        PP.PathPart { PP.pathPartPath     = pathPk
                    , PP.pathPartIndex    = idx
                    , PP.pathPartVenue    = VenueId (toS venue)
                    , PP.pathPartCurrency = CurrencyId (toS currency)
                    }
