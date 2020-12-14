{-# LANGUAGE GADTs #-}

module Insert.Paths where

import           Data.List                                ( sort )
import qualified Data.List.NonEmpty                       as NE

import           Database
import           Database.Beam
import           Database.Beam.Backend
                 ( BeamSqlBackend, BeamSqlBackendSyntax, HasSqlValueSyntax
                 , Sql92ExpressionValueSyntax, Sql92SelectSelectTableSyntax
                 , Sql92SelectSyntax, Sql92SelectTableExpressionSyntax )
import           Database.Beam.Backend.SQL.BeamExtensions
                 ( MonadBeamInsertReturning(runInsertReturningList) )

import           Internal.Prelude

import qualified OrderBook.Graph                          as G

import           Schema.Calculation
import           Schema.Currency
                 ( CurrencyId, CurrencyT, PrimaryKey(CurrencyId) )
import qualified Schema.Path                              as Path
import qualified Schema.PathPart                          as PP
import qualified Schema.PathQty                           as PQty
import           Schema.Venue
                 ( PrimaryKey(VenueId), VenueId )

-- Path
--     { pathId
--     , pathLength
--     , pathStart
--     }
-- PathPart
--     { pathPartPath
--     , pathPartIndex
--     , pathPartVenue
--     , pathPartCurrency
--     }
-- PathQty
--     { pathQtyCalc
--     , pathQtyPath
--     , pathQtyQty
--     , pathQtyPriceLow
--     , pathQtyPriceHigh
--     }

insertAllPaths calcPk pathList =
    let hey = groupOn G.pathDescr pathList
        lol = map (\lst ->
                   (G.pathDescr $ head lst, (map G.pQty lst, map G.pPrice lst)))
                  hey
    in
        forM_ lol $ \(pathDescr, (pathQtyLst, pathPrices)) ->
        insertSinglePath calcPk
                         pathDescr
                         (round $ sum pathQtyLst)
                         (sort pathPrices)

insertSinglePath calcPk pathDescr pathQty pathPrices = do
    pathT <- runInsertReturningOne pathInsert
    runInsert $ pathPartInsert (pk pathT)
    runInsert $ pathQtyInsert (pk pathT)
  where
    pathInsert = insert (paths liquidityDb) $
        insertExpressions [ Path.Path default_
                                      (val_ . fromIntegral $ length pathEdges)
                                      (CurrencyId (val_ . toS $
                                                   G.pStart pathDescr))
                          ]

    pathLookup pathParts = let tmp = pathParts :: [PP.PathPart]
                               lol = 4
                           in
                               undefined

    -- PathPart
    --     { pathPartPath
    --     , pathPartIndex
    --     , pathPartVenue
    --     , pathPartCurrency
    --     }
    pathEdges = G.pMoves pathDescr

    pathPartInsert pathPk = insert (pathParts liquidityDb) $ insertValues $
        for (zip [ 0 .. ] (NE.toList pathEdges)) $ \(idx, (venue, currency)) ->
        PP.PathPart { PP.pathPartPath     = pathPk
                    , PP.pathPartIndex    = idx
                    , PP.pathPartVenue    = VenueId (toS venue)
                    , PP.pathPartCurrency = CurrencyId (toS currency)
                    }

    pathQtyInsert pathPk = insert (pathQtys liquidityDb) $
        insertValues [ PQty.PathQty { PQty.pathQtyCalc      = calcPk
                                    , PQty.pathQtyPath      = pathPk
                                    , PQty.pathQtyQty       = pathQty
                                    , PQty.pathQtyPriceLow  = head pathPrices
                                    , PQty.pathQtyPriceHigh = last pathPrices
                                    }
                     ]
