{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Query.Graph
( toGraphData
, GraphData
)
where

import Internal.Prelude

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

import Control.Monad.ST (ST, runST)
import Data.Maybe (fromMaybe)


-- |
data Edge = Edge
    Text -- ^ venue
    Currency -- ^ src
    Currency -- ^ dst
    PathQty.Int64 -- ^ quantity

toGraphData :: (Run.Run, [(Text, PathQty.Int64, Path.Path)]) -> GraphData
toGraphData (run', input) =
    runST $ toGraphOutput input >>= fromGraphData run'

toGraphOutput
    :: [(Text, PathQty.Int64, Path.Path)]
    -> ST s (DG.Digraph s Currency [(Text, PathQty.Int64)])
toGraphOutput input =
    DG.fromEdgesCombine (\lstM item -> item : fromMaybe [] lstM)
                        (concatMap toEdges input)
  where
    toEdges :: (Text, PathQty.Int64, Path.Path) -> [Edge]
    toEdges (_, pathQty, path) = snd $
        foldr (\(venue, dst) (src, lst) ->
                let edge = Edge venue (toS src) (toS dst) pathQty
                in (dst, edge : lst)
              )
              (getSymbol $ Path.pathStart path, [])
              (Vec.zip (Path.pathVenues path) (Path.pathCurrencys path))

fromGraphData
    :: forall s.
       Run.Run
    -> DG.Digraph s Currency [(Text, PathQty.Int64)]
    -> ST s GraphData
fromGraphData run' graph = do
    nodes' <- nodesQuantitiesM
    edges' <- edgesM
    return $ GraphData
        { nodes = Vec.fromList nodes'
        , links = Vec.fromList edges'
        , run = run'
        }
  where
    vertexQuantity vid = do
        iEdges <- DG.incomingEdges graph vid
        oEdges <- DG.outgoingEdges graph vid
        return $ sum $ concatMap (map snd . DG.eMeta) $ iEdges ++ oEdges

    nodesQuantitiesM = do
        nodes' <- DG.vertexLabelsId graph
        let addQuantity (v, idx) = vertexQuantity idx >>= \qty' ->
                pure (JsonNode v (DG.vidInt idx) (fromIntegral qty'))
        mapM addQuantity nodes'

    edgesM = do
        edges' <- DG.edges graph
        let fromIdxEdge ie =
                let (src, dst) = sourceTarget ie
                    (size', venues') = sizeVenues ie
                in JsonEdge src dst size' venues'
            sourceTarget ie = (DG.vidInt $ DG.eFromIdx ie, DG.vidInt $ DG.eToIdx ie)
            sizeVenues ie =
                let lst = DG.eMeta ie
                in ( fromIntegral . sum $ map snd lst
                   , T.intercalate (fromString ",") $ map fst lst
                   )
        return $ map fromIdxEdge edges'


instance DG.DirectedEdge Edge Currency (Text, PathQty.Int64) where
    fromNode (Edge _ from _ _) = from
    toNode (Edge _ _ to _) = to
    metaData (Edge venue _ _ qty') = (venue, qty')

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
