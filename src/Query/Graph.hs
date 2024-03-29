{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Query.Graph
( toGraphData
, GraphData(..)
, JsonNode(..)
, JsonEdge(..)
)
where

import Internal.Prelude
import Query.Config.Config (numeraires)
import Protolude (Hashable, sort)

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

-- |
toGraphData
    :: Currency
    -> Maybe Word -- limit number of returned currencies
    -> [(Currency, PathQty.Int64)]
    -> (Run.Run, [(Text, PathQty.Int64, Path.Path)])
    -> GraphData
toGraphData numeraire numCurrenciesM currencyQtys (run', input) =
    let topQtyCurrenciesM = fmap ((`take` allCurrenciesSorted) . fromIntegral) numCurrenciesM
        allCurrenciesSorted = map fst $ sortBy (\(_, a) (_, b) -> b `compare` a) currencyQtys
        -- we want the numeraire to be in the graph, and therefore we need it in the "quantity map".
        -- but its quantity is undefined, so we just set it to -1 here.
        qtyMap = Map.insert numeraire (-1) $ Map.fromList currencyQtys
        topQtyCurrenciesM' = fmap (numeraire :) topQtyCurrenciesM
    in runST $ toGraphOutput input >>=
        fromGraphData topQtyCurrenciesM' qtyMap run'

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
       Maybe [Currency]
    -> Map.HashMap Currency PathQty.Int64
    -> Run.Run
    -> DG.Digraph s Currency QtyMap
    -> ST s GraphData
fromGraphData topCurrenciesM qtyMap run' graph = do
    nodes' <- nodesQuantitiesM
    edges' <- edgesM
    return $ GraphData
        { nodes = Vec.fromList nodes'
        , links = Vec.fromList edges'
        , run = run'
        }
  where
    nodesQuantitiesM = do
        nodes' <- filterNodes <$> DG.vertexLabelsId graph
        let addQuantity (v, idx') =
                let quantity = fromMaybe (error errMsg) (Map.lookup v qtyMap)
                    errMsg = "BUG: fromGraphData: missing currency " ++ toS v
                    isCrypto = toS v `notElem` numeraires
                in do
                    edgeList <- DG.incomingEdges graph idx'
                    let venues' = sort $ Set.toList $ Set.unions $ map Map.keysSet $ concatMap (Map.elems . DG.eMeta) edgeList
                    pure $ JsonNode v (DG.vidInt idx') (fromIntegral quantity) isCrypto (length edgeList) venues'
        mapM addQuantity nodes'

    edgesM = do
        edges' <- filterEdges <$> DG.edges graph
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
                   , map fst venuesQtys
                   )
        return $ map fromIdxEdge edges'

    -- helpers

    containsSrcDstOf topCurrencies edge =
        DG.eFrom edge `elem` topCurrencies && DG.eTo edge `elem` topCurrencies

    filterEdges = maybe id (filter . containsSrcDstOf) topCurrenciesM

    filterNodes = maybe id (\topCurrencies -> filter ((`elem` topCurrencies) . fst)) topCurrenciesM


instance DG.DirectedEdge Edge Currency (Currency, Text, PathQty.Int64) where
    fromNode (Edge _ from _ _) = from
    toNode (Edge _ _ to _) = to
    metaData (Edge (venue, currency) _ _ qty') = (currency, venue, qty')

data JsonNode = JsonNode
    { name :: Currency
    , idx :: Int -- "index" is reserved by "d3-force"
    , qty :: Integer
    , is_crypto :: Bool
    , market_count :: Int
    , node_venues :: [Text]
    } deriving (Eq, Show, Generic)

data JsonEdge = JsonEdge
    { source :: Int
    , target :: Int
    , size :: Integer
    , venues :: [Text]
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
