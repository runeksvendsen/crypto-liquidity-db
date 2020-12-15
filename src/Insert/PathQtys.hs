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
import           Schema.Currency
                 ( PrimaryKey(CurrencyId) )
import qualified Schema.Path                              as Path
import qualified Schema.PathPart                          as PP
import qualified Schema.PathQty                           as PQty
import           Schema.Venue
                 ( PrimaryKey(VenueId) )
import Data.Word (Word16)


type Selectable be = HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax (BeamSqlBackendSyntax be)))))

insertAllPathQtys
    :: ( G.HasPathQuantity path Rational
       , HasSqlEqualityCheck be Text
       , HasSqlEqualityCheck be PP.Int16
       , HasSqlEqualityCheck be Word16
       , HasSqlEqualityCheck be Path.Word32
       , HasQBuilder be
       , MonadBeamInsertReturning be m
       , FromBackendRow be Path.Word32
       , FromBackendRow be Word16
       , FromBackendRow be Text
       , Selectable be Text
       , Selectable be Path.Word32
       , Selectable be Integer
       , Selectable be Double
       , Selectable be Word16
       , Selectable be PP.Int16
       , Selectable be PQty.Word64
       , MonadIO m
       )
    => PrimaryKey Calc.CalculationT Identity
    -> [path]
    -> m ()
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
    pathT <- pathLookupOrInsert pathDescr
    runInsert $ pathQtyInsert (pk pathT)
  where
    pathQtyInsert pathPk = insert (pathQtys liquidityDb) $
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
    let pathParts' = mkPathParts (Path.PathId 0) -- we use a dummy path ID (which is ignored by "pathLookup")
    pathTM <- runSelectReturningOne $ select $ pathLookup pathParts'
    case pathTM of
        Just pathT -> do
            liftIO $ putStrLn $ "Found existing path " ++ show (pk pathT)
            return pathT
        Nothing -> do
            pathT <- runInsertReturningOne pathInsert
            runInsert $ pathPartInsert (pk pathT)
            liftIO $ putStrLn $ "Inserting new path " ++ show (pk pathT)
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
        guard_ $ Path.pathLength path
            ==. val_ (fromIntegral $ length pathPartLst)
        pathPart <- partsForPath path
        guard_ $ foldr (\pathPart' state -> state ||. PP.pathpartIndex pathPart
                        ==. val_ (PP.pathpartIndex pathPart')
                        ||. PP.pathpartVenue pathPart
                        ==. val_ (PP.pathpartVenue pathPart')
                        ||. PP.pathpartCurrency pathPart
                        ==. val_ (PP.pathpartCurrency pathPart'))
                       (val_ True)
                       pathPartLst
        pure path

    pathPartInsert = insert (pathParts liquidityDb) . insertValues . mkPathParts

    mkPathParts pathPk = for (zip [ 0 .. ] (NE.toList pathEdges)) $
        \(idx, (venue, currency)) ->
        PP.PathPart { PP.pathpartPath     = pathPk
                    , PP.pathpartIndex    = idx
                    , PP.pathpartVenue    = VenueId (toS venue)
                    , PP.pathpartCurrency = CurrencyId (toS currency)
                    }
