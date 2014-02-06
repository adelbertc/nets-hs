module Nets.Graph.Graph
    (
        Graph,
        addVertex,
        addVertexIfMissing,
        degree,
        hasVertex,
        neighbors,
        order,
        vertexSet,
        vertices,
        addEdge,
        edges,
        getEdge,
        hasEdge,
        size,
        weightOf,
        isDirected,
        isUndirected,
        bfs,
        hasPath
    ) where

import qualified Data.IntMap as IM
import qualified Data.Map as DM
import qualified Data.Maybe as M
import qualified Data.Set as S
import Prelude hiding (reverse)

import Nets.Graph.Edge
import Nets.Graph.Vertex
import qualified Nets.Util.Queue as Q

type Nbors w = S.Set (Edge w)
type AdjList w = IM.IntMap (Nbors w)
data Graph w = DGraph (AdjList w) | UGraph (AdjList w)

-- Vertex centric functions
addVertex :: Vertex -> Graph w -> Graph w
addVertex (Vertex u) = mapAdj insertVertex
    where insertVertex = IM.insert u S.empty

addVertexIfMissing :: Vertex -> Graph w -> Graph w
addVertexIfMissing (Vertex u) = mapAdj insertVertex
    where insertVertex a = IM.union a $ IM.singleton u S.empty

degree :: Vertex -> Graph w -> Maybe Int
degree (Vertex u) g = fmap S.size $ IM.lookup u (adjList g)

hasVertex :: Vertex -> Graph w -> Bool
hasVertex (Vertex u) g = IM.member u (adjList g)

neighbors :: Vertex -> Graph w -> Maybe (Nbors w)
neighbors (Vertex u) g = IM.lookup u (adjList g)

order :: Graph w -> Int
order = IM.size . adjList

vertexSet :: Graph w -> S.Set Vertex
vertexSet = S.fromList . vertices

vertices :: Graph w -> [Vertex]
vertices = fmap Vertex . IM.keys . adjList

-- Edge centric functions
addEdge :: Edge w -> Graph w -> Graph w
addEdge e g = mapAdj insertEdge g
    where oneWay = IM.singleton (value $ from e) (S.singleton e)

          toInsert = if isUndirected g then IM.insert (value $ to e) (S.singleton (reverse e)) oneWay
                     else oneWay

          insertEdge a = IM.unionWith S.union a toInsert

edges :: Graph w -> S.Set (Edge w)
edges g = S.fromList $ filter predicate $ allEdges g
    where predicate = if isUndirected g then \_ -> True
                      else \e -> let (f, t) = endpoints e in f <= t

getEdge :: (Vertex, Vertex) -> Graph w -> Maybe (Edge w)
getEdge e@(u, _) g = do
    a <- IM.lookup (value u) $ adjList g
    f <- return $ S.filter (\nbor -> endpoints nbor == e) a
    if S.null f then Nothing else Just $ head (S.toList f)

hasEdge :: (Vertex, Vertex) -> Graph w -> Bool
hasEdge e g = M.isJust $ getEdge e g

size :: Graph w -> Int
size g = case g of
            DGraph _ -> bothDirections
            UGraph _ -> bothDirections `div` 2
         where bothDirections = length $ allEdges g

weightOf :: (Vertex, Vertex) -> Graph w -> Maybe w
weightOf e g = fmap weight $ getEdge e g

-- Graph functions
isDirected :: Graph w -> Bool
isDirected (DGraph _) = True
isDirected (UGraph _) = False

isUndirected :: Graph w -> Bool
isUndirected (DGraph _) = False
isUndirected (UGraph _) = True

bfs :: Vertex -> Graph w -> Maybe (DM.Map Vertex Int)
bfs r g = bfsAux g (DM.singleton r 0) (Q.singleton r)

bfsAux :: Graph w -> DM.Map Vertex Int -> Q.Queue Vertex -> Maybe (DM.Map Vertex Int)
bfsAux g m q = if not $ Q.null q then recurse else Just m
    where recurse = do (u, rest) <- Q.dequeue q
                       pathLen   <- DM.lookup u m
                       nbors     <- neighbors u g
                       let (m', q') =  bfsStep m rest nbors (pathLen + 1)
                       bfsAux g m' q'

bfsStep :: DM.Map Vertex Int -> Q.Queue Vertex -> Nbors w -> Int -> (DM.Map Vertex Int, Q.Queue Vertex)
bfsStep m q ns x = S.foldr step (m, q) ns
    where step n p@(m', q') = if DM.member (to n) m then p
                            else (DM.insert (to n) x m', Q.enqueue (to n) q')

hasPath :: [Vertex] -> Graph w -> Bool
hasPath ns@(_:_:_) g = length (takeWhile (`hasEdge` g) edgePath) == length edgePath
    where edgePath = zip ns (drop 1 ns)
hasPath [u] g = hasVertex u g
hasPath [] _ = True

-- Helper functions
adjList :: Graph w -> AdjList w
adjList (DGraph a) = a
adjList (UGraph a) = a

allEdges :: Graph w -> [Edge w]
allEdges g = IM.elems (adjList g) >>= S.toList

mapAdj :: (AdjList w -> AdjList w) -> Graph w -> Graph w
mapAdj f (DGraph a) = DGraph $ f a
mapAdj f (UGraph a) = UGraph $ f a
