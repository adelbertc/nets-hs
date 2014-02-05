module Nets.Graph.Graph
    (
        Graph
    ) where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Maybe as M
import qualified Data.Set as S
import Prelude hiding (reverse)

import Nets.Graph.Edge

type Nbors w = S.Set (Edge w)
type AdjList w = IM.IntMap (Nbors w)
data Graph w = DGraph (AdjList w) | UGraph (AdjList w)

-- Vertex centric functions
addVertex :: Int -> Graph w -> Graph w
addVertex u g = mapAdj insertVertex g
    where insertVertex = IM.insert u (S.empty)

addVertexIfMissing :: Int -> Graph w -> Graph w
addVertexIfMissing u g = mapAdj insertVertex g
    where insertVertex a = IM.union a $ IM.singleton u S.empty

degree :: Int -> Graph w -> Maybe Int
degree u g = fmap S.size $ IM.lookup u (adjList g)

hasVertex :: Int -> Graph w -> Bool
hasVertex u g = IM.member u (adjList g)

neighbors :: Int -> Graph w -> Maybe (Nbors w)
neighbors u g = IM.lookup u (adjList g)

order :: Graph w -> Int
order = IM.size . adjList

vertexSet :: Graph w -> IS.IntSet
vertexSet = IM.keysSet . adjList

vertices :: Graph w -> [Int]
vertices = IM.keys . adjList

-- Edge centric functions
addEdge :: Edge w -> Graph w -> Graph w
addEdge e g = mapAdj insertEdge g
    where oneWay = IM.singleton (from e) (S.singleton e)

          toInsert = if isUndirected g then IM.insert (to e) (S.singleton (reverse e)) oneWay
                     else oneWay

          insertEdge a = IM.unionWith S.union a toInsert

edges :: Graph w -> S.Set (Edge w)
edges g = S.fromList $ filter predicate $ allEdges g
    where predicate = if isUndirected g then \_ -> True
                      else \e -> let (f, t) = endpoints e in f <= t

getEdge :: (Int, Int) -> Graph w -> Maybe (Edge w)
getEdge e@(u, v) g = do
    a <- IM.lookup u $ adjList g
    f <- return $ S.filter (\nbor -> endpoints nbor == e) a
    if S.null f then Nothing else Just $ head (S.toList f)

hasEdge :: (Int, Int) -> Graph w -> Bool
hasEdge e g = M.isJust $ getEdge e g

size :: Graph w -> Int
size g = case g of
            DGraph _ -> bothDirections
            UGraph _ -> bothDirections `div` 2
         where bothDirections = length $ allEdges g

weightOf :: (Int, Int) -> Graph w -> Maybe w
weightOf e g = fmap weight $ getEdge e g

-- Graph functions
isDirected :: Graph w -> Bool
isDirected (DGraph _) = True
isDirected (UGraph _) = False

isUndirected :: Graph w -> Bool
isUndirected (DGraph _) = False
isUndirected (UGraph _) = True

-- Helper functions
adjList :: Graph w -> AdjList w
adjList (DGraph a) = a
adjList (UGraph a) = a

allEdges :: Graph w -> [Edge w]
allEdges g = IM.elems (adjList g) >>= S.toList

mapAdj :: (AdjList w -> AdjList w) -> Graph w -> Graph w
mapAdj f (DGraph a) = DGraph $ f a
mapAdj f (UGraph a) = UGraph $ f a
