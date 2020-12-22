{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Insert.PathQtys ( insertAllPathQtys ) where

import           Data.List                                ( sort )
import qualified Data.List.NonEmpty                       as NE

import           Database
import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Backend.SQL.BeamExtensions

import           Internal.Prelude

import qualified OrderBook.Graph                          as G

import qualified Schema.Calculation                       as Calc
import qualified Schema.Currency                          as Currency
import qualified Schema.Venue                          as Venue

import qualified Schema.Path                              as Path
import qualified Schema.PathPart                          as PP
import qualified Schema.PathQty                           as PQty
import           Schema.Venue
                 ( PrimaryKey(VenueId) )
import Data.Word (Word16)


insertAllPathQtys calcPk pathList =
    let groupedPaths = groupOn G.pathDescr pathList
        pathPriceQtyLst =
            map (\lst ->
                 (G.pathDescr $ head lst, (map G.pQty lst, map (realToFrac . G.pPrice) lst)))
                groupedPaths
    in
        forM_ pathPriceQtyLst $ \(pathDescr, (pathQtyLst, pathPrices)) ->
        insertSinglePathQty calcPk
                            pathDescr
                            (round $ sum pathQtyLst)
                            (sort pathPrices)

insertSinglePathQty calcPk pathDescr pathQty sortedPathPrices = do
    pathPk <- pathLookupOrInsert pathDescr
    runInsert $ pathQtyInsert pathPk
  where
    pathQtyInsert pathPk = insert (path_qtys liquidityDb) $
        insertValues [ PQty.PathQty { PQty.pathqtyCalc      = calcPk
                                    , PQty.pathqtyPath      = pathPk
                                    , PQty.pathqtyQty       = pathQty
                                    , PQty.pathqtyPriceLow  =
                                          head sortedPathPrices
                                    , PQty.pathqtyPriceHigh =
                                          last sortedPathPrices
                                    }
                     ]

pathLookupOrInsert pathDescr = do
    pathTM <- runSelectReturningOne $ select $ pathLookup pathDescr
    case pathTM of
        Just pathPk -> do
            liftIO $ putStrLn $ "Found existing path " ++ toS (G.showPath pathDescr)
            return pathPk
        Nothing -> do
            pathT <- runInsertReturningOne pathInsert
            runInsert $ pathPartInsert (pk pathT)
            liftIO $ putStrLn $ "Inserting new path " ++ toS (G.showPath pathDescr)
            return (pk pathT)
  where
    pathEdges = G.pMoves pathDescr

    pathInsert = insert (paths liquidityDb) $
        insertExpressions [ Path.Path default_
                                      (val_ . fromIntegral $ length pathEdges)
                                      (Currency.CurrencyId (val_ . toS $
                                                   G.pStart pathDescr))
                          ]

    pathPartInsert = insert (path_parts liquidityDb) . insertValues . mkPathParts pathEdges

    mkPathParts pathEdges' pathPk = for (zip [ 0 .. ] (NE.toList pathEdges')) $
        \(idx, (venue, currency)) ->
        PP.PathPart { PP.pathpartPath     = pathPk
                    , PP.pathpartIndex    = idx
                    , PP.pathpartVenue    = VenueId (toS venue)
                    , PP.pathpartCurrency = Currency.CurrencyId (toS currency)
                    }

pathLookup pathDescr = fmap fst $
    filter_ (\(_, count) -> count ==. val_ moveCount ) $
    aggregate_ (\path -> (group_ (pk path), countAll_)) $ do
        path <- all_ (paths liquidityDb)
        pathPart <- partsForPath path
        guard_ $
            Path.pathStart path ==. val_ (Currency.CurrencyId $ toS $ G.pStart pathDescr) &&.
            Path.pathPartCount path ==. val_ moveCount &&.
            foldr (\move state -> state ||. pathPart `equals` move) (val_ False) (zip [0..] moveList)
        pure path
  where
    moveCount = fromIntegral $ length moveList
    currencyIdSymbol (Currency.CurrencyId symbol) = symbol
    venueIdSymbol (Venue.VenueId venue) = venue
    moveList = NE.toList $ G.pMoves pathDescr
    equals pathPart (idx, (venue', currency')) =
        PP.pathpartIndex pathPart ==. val_ idx
        &&. venueIdSymbol (PP.pathpartVenue pathPart) ==. val_ (toS venue')
        &&. currencyIdSymbol (PP.pathpartCurrency pathPart) ==. val_ (toS currency')
