{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Insert.PathQtys ( insertAllPathQtys ) where

import App.Monad (asTx)

import           Data.List                                ( sort )
import qualified Data.List.NonEmpty                       as NE

import           Database
import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Backend.SQL.BeamExtensions

import           Internal.Prelude

import qualified OrderBook.Graph                          as G

import qualified Schema.Calculation                       as Calc
import qualified Schema.PathSum
import qualified Schema.Currency                          as Currency
import qualified Schema.Venue                          as Venue

import qualified Schema.Path                              as Path
import qualified Schema.PathQty                           as PQty
import           Schema.Venue
                 ( PrimaryKey(VenueId) )
import Data.Int (Int16)
import qualified Data.Vector as Vec


insertPathSum calcPk buyPaths sellPaths =
    insert (path_sums liquidityDb) $ insertValues
        [ Schema.PathSum.PathSum
            { Schema.PathSum.pathsumCalc = calcPk
            , Schema.PathSum.pathsumBuyQty = pathsSum buyPaths
            , Schema.PathSum.pathsumSellQty = pathsSum sellPaths
            }
        ]
  where
    pathsSum :: G.HasPathQuantity path Rational => [path] -> PQty.Int64
    pathsSum = round . sum . map G.pQty

insertAllPathQtys calcPk buyPaths sellPaths = asTx $
    let mkPathPriceQtyLst pathList =
            map (\lst ->
                 (G.pathDescr $ head lst, (map G.pQty lst, map (realToFrac . G.pPrice) lst)))
                (groupOn G.pathDescr pathList)
        allPaths = mkPathPriceQtyLst buyPaths ++ mkPathPriceQtyLst sellPaths
    in do
        forM_ allPaths $ \(pathDescr, (pathQtyLst, pathPrices)) ->
            insertSinglePathQty calcPk
                                pathDescr
                                (round $ sum pathQtyLst)
                                (sort pathPrices)
        runInsert $ insertPathSum calcPk buyPaths sellPaths

insertSinglePathQty calcPk pathDescr pathQty sortedPathPrices = do
    pathPk <- pathInsert pathDescr
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

pathInsert pathDescr =
    pk <$> runInsertReturningOne pathInsert'
  where
    pathEdges = G.pMoves pathDescr
    (venues', currencys') = unzip (NE.toList pathEdges)

    pathInsert' = insert (paths liquidityDb) $
        insertExpressions [ Path.Path default_
                                      (Currency.CurrencyId (val_ . toS $
                                                   G.pStart pathDescr))
                                      (val_ $ Vec.fromList venues')
                                      (val_ $ Vec.fromList (map toS currencys'))
                          ]
