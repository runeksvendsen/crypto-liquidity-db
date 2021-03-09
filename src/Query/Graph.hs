{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Query.Graph
( toGraphData
, GraphData
)
where

import Internal.Prelude
import Protolude (Hashable)

-- crypto-liquidity-db
import App.Main.WebApi.Orphans ()
import qualified Schema.Path as Path
import qualified Schema.PathQty as PathQty
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run
import Schema.Currency (getSymbol)

-- bellman-ford
import qualified Data.Graph.Digraph as DG

-- order-graph
import OrderBook.Graph.Types.Currency (Currency)

-- vector
import qualified Data.Vector as Vec

-- aeson
import qualified Data.Aeson as Json

-- text
import qualified Data.Text as T

-- unordered-containers
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

import Control.Monad.ST (ST, runST)
import Data.Maybe (fromMaybe)
import Data.List (sortBy)


-- |
data Edge = Edge
    (Text, Currency) -- ^ (venue, crypto)
    Currency -- ^ src
    Currency -- ^ dst
    PathQty.Int64 -- ^ quantity

toGraphData
    :: Maybe Word
    -> [(Currency, PathQty.Int64)]
    -> (Run.Run, [(Text, PathQty.Int64, Path.Path)])
    -> GraphData
toGraphData numCurrenciesM currencyQtys (run', input) =
    let topQtyCurrencies = maybe id (take . fromIntegral) numCurrenciesM $
            map fst $ sortBy (\(_, a) (_, b) -> b `compare` a) currencyQtys
    in runST $ toGraphOutput input >>= fromGraphData topQtyCurrencies (Map.fromList currencyQtys) run'

type QtyMap = Map.HashMap Currency (Map.HashMap Text PathQty.Int64)

toGraphOutput
    :: [(Text, PathQty.Int64, Path.Path)] -- (currency, qty, path)
    -> ST s (DG.Digraph s Currency QtyMap)
toGraphOutput input =
    DG.fromEdgesCombine (\maybeMap (currency, venue, qty') ->
                            Map.insertWith (Map.unionWith (+))
                                           currency
                                           (Map.singleton venue qty')
                                           (fromMaybe Map.empty maybeMap)
                        )
                        (concatMap toEdges input)
  where
    toEdges :: (Text, PathQty.Int64, Path.Path) -> [Edge]
    toEdges (crypto, pathQty, path) = snd $
        foldr (\(venue, dst) (src, lst) ->
                let edge = Edge (venue, toS crypto) (toS src) (toS dst) pathQty
                in (dst, edge : lst)
              )
              (getSymbol $ Path.pathStart path, [])
              (Vec.zip (Path.pathVenues path) (Path.pathCurrencys path))

fromGraphData
    :: forall s.
       [Currency]
    -> Map.HashMap Currency PathQty.Int64
    -> Run.Run
    -> DG.Digraph s Currency QtyMap
    -> ST s GraphData
fromGraphData topCurrencies qtyMap run' graph = do
    nodes' <- nodesQuantitiesM
    edges' <- edgesM
    return $ GraphData
        { nodes = Vec.fromList nodes'
        , links = Vec.fromList edges'
        , run = run'
        }
  where
    nodesQuantitiesM = do
        nodes' <- fastIntersectionWith const (map (,()) topCurrencies) <$> DG.vertexLabelsId graph
        let addQuantity (v, idx) =
                let qty' = fromIntegral $ fromMaybe (error errMsg) (Map.lookup v qtyMap)
                    errMsg = "BUG: fromGraphData: missing currency " ++ toS v
                in pure (JsonNode v (DG.vidInt idx) qty')
        mapM addQuantity nodes'

    edgesM = do
        edges' <- filter topCurrencyRelated <$> DG.edges graph
        let fromIdxEdge ie =
                let (src, dst) = sourceTarget ie
                    (size', venues') = sizeVenues ie
                in JsonEdge src dst size' venues'
            sourceTarget ie = (DG.vidInt $ DG.eFromIdx ie, DG.vidInt $ DG.eToIdx ie)
            sizeVenues ie =
                let hmap = DG.eMeta ie :: QtyMap
                    mapQty :: Map.HashMap Text PathQty.Int64 -> PathQty.Int64
                    mapQty = sum . Map.elems
                    highestVolumeMap = last . sortOn mapQty
                    venuesQtys = Map.toList $ highestVolumeMap $ map snd (Map.toList hmap)
                in ( fromIntegral . sum $ map snd venuesQtys
                   , T.intercalate (fromString ",") $ map fst venuesQtys
                   )
        return $ map fromIdxEdge edges'

    -- helpers

    topCurrencyRelated edge =
        DG.eFrom edge `elem` topCurrencies &&  DG.eFrom edge `elem` topCurrencies

    -- fastIntersectionWith
    --     :: (Eq k, Hashable k)
    --     => (v1 -> v2 -> v3)
    --     -> [(Currency, v1)]
    --     -> [(Currency, v3)]
    fastIntersectionWith f secondary primary = Map.toList $
        Map.intersectionWith f (Map.fromList primary) (Map.fromList secondary)

instance DG.DirectedEdge Edge Currency (Currency, Text, PathQty.Int64) where
    fromNode (Edge _ from _ _) = from
    toNode (Edge _ _ to _) = to
    metaData (Edge (venue, currency) _ _ qty') = (currency, venue, qty')

data JsonNode = JsonNode
    { name :: Currency
    , index :: Int
    , qty :: Integer
    } deriving (Eq, Show, Generic)

data JsonEdge = JsonEdge
    { source :: Int
    , target :: Int
    , size :: Integer
    , venues :: Text
    } deriving (Eq, Show, Generic)

data GraphData = GraphData
    { nodes :: Vec.Vector JsonNode
    , links :: Vec.Vector JsonEdge
    , run :: Run.Run
    } deriving (Eq, Show, Generic)

instance Json.ToJSON JsonNode
instance Json.FromJSON JsonNode
instance Json.ToJSON JsonEdge
instance Json.FromJSON JsonEdge
instance Json.ToJSON GraphData
instance Json.FromJSON GraphData
