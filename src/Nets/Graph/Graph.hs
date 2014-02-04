module Nets.Graph.Graph
    (
        Graph
    ) where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S

import Nets.Graph.Edge

type Nbors w = S.Set (Edge w)
type AdjList w = IM.IntMap (Nbors w)
data Graph w = DGraph (AdjList w) | UGraph (AdjList w)

-- Vertex centric functions
addVertex :: Int -> Graph w -> Graph w
addVertex u g = case g of
                    DGraph _ -> DGraph $ newAdj
                    UGraph _ -> UGraph $ newAdj
                where newAdj = IM.insert u (S.empty) $ adjList g

addVertexIfMissing :: Int -> Graph w -> Graph w
addVertexIfMissing u g = case g of
                            DGraph _ -> DGraph $ newAdj
                            UGraph _ -> UGraph $ newAdj
                         where newAdj = IM.union (adjList g) (IM.singleton u S.empty)

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

-- Helper functions
adjList :: Graph w -> AdjList w
adjList (DGraph a) = a
adjList (UGraph a) = a
