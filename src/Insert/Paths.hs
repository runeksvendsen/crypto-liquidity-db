{-# LANGUAGE GADTs #-}
module Insert.Paths where

import Internal.Prelude
import Database
import qualified Schema.Path as Path
import qualified Schema.PathPart as PP
import qualified Schema.PathQty as PQty

import qualified OrderBook.Graph as G

import qualified Data.List.NonEmpty as NE
import Database.Beam
import Schema.Venue (PrimaryKey(VenueId), VenueId)
import Schema.Currency (CurrencyT, PrimaryKey(CurrencyId), CurrencyId)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning(runInsertReturningList))
import Database.Beam.Backend (BeamSqlBackendSyntax, Sql92SelectSyntax, Sql92SelectSelectTableSyntax, Sql92SelectTableExpressionSyntax, Sql92ExpressionValueSyntax, HasSqlValueSyntax, BeamSqlBackend)
import Data.List (sort)
import Schema.Calculation


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


insertAllPaths
    :: ( MonadBeam be m, FromBackendRow be Text, G.HasPathQuantity path Double, HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Text, HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Path.Word32, HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Double, HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Word, HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be))))) Integer, FromBackendRow be Word, MonadBeamInsertReturning be m, BeamSqlBackend be)
    => CalculationId
    -> [path]
    -> m ()
insertAllPaths calcPk pathList =
    let hey = groupOn G.pathDescr pathList
        lol = map (\lst -> (G.pathDescr $ head lst, (map G.pQty lst, map G.pPrice lst))) hey
    in forM_ lol $ \(pathDescr, (pathQtyLst, pathPrices)) ->
            insertSinglePath calcPk pathDescr (round $ sum pathQtyLst) (sort pathPrices)

insertSinglePath calcPk pathDescr pathQty pathPrices = do
    pathT <- runInsertReturningOne pathInsert
    runInsert $ pathPartInsert (pk pathT)
    runInsert $ pathQtyInsert (pk pathT)
  where
    pathInsert =
        insert (paths liquidityDb) $
            insertExpressions
                [ Path.Path
                    default_
                    (val_ . fromIntegral $ length pathEdges)
                    (CurrencyId (val_ . toS $ G.pStart pathDescr))
                ]
    pathLookup pathParts =
        let tmp = pathParts :: [PP.PathPart]
            lol = 4
        in undefined
-- PathPart
--     { pathPartPath
--     , pathPartIndex
--     , pathPartVenue
--     , pathPartCurrency
--     }
    pathEdges = G.pMoves pathDescr
    pathPartInsert pathPk =
        insert (pathParts liquidityDb) $
        insertValues $ for (zip [0..] (NE.toList pathEdges)) $ \(idx, (venue, currency)) ->
            PP.PathPart
                { PP.pathPartPath = pathPk
                , PP.pathPartIndex = idx
                , PP.pathPartVenue = VenueId (toS venue)
                , PP.pathPartCurrency = CurrencyId (toS currency)
                }
    pathQtyInsert pathPk =
        insert (pathQtys liquidityDb) $
        insertValues
            [ PQty.PathQty
                { PQty.pathQtyCalc = calcPk
                , PQty.pathQtyPath = pathPk
                , PQty.pathQtyQty = pathQty
                , PQty.pathQtyPriceLow = head pathPrices
                , PQty.pathQtyPriceHigh = last pathPrices
                }
            ]
