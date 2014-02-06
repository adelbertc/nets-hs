module Nets.Graph
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
        bfs,
        cost,
        directed,
        hasPath,
        isDirected,
        isUndirected,
        stronglyConnected,
        undirected,
        weaklyConnected,
        emptyD,
        emptyU,
        fromEdgesD,
        fromEdgesU,
        nullD,
        nullU,
        readAdjacencyListU,
        readAdjacencyListW,
        writeAdjacencyListU,
        writeAdjacencyListW,
        writeEdgelistU,
        writeEdgelistW
    ) where

import qualified Control.Applicative as A
import Data.Functor ((<$>))
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import qualified Data.Map as DM
import qualified Data.Maybe as Ma
import qualified Data.Monoid as Mo
import qualified Data.Set as S
import Prelude hiding (reverse)
import qualified System.IO as IO

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Char8 as C8

import Nets.Edge
import Nets.Vertex
import qualified Nets.Util.Queue as Q

type Nbors w = S.Set (Edge w)
type AdjList w = IM.IntMap (Nbors w)
data Graph w = DGraph (AdjList w) | UGraph (AdjList w) deriving (Eq)

-- Vertex centric functions
addVertex :: Graph w -> Vertex -> Graph w
addVertex g (Vertex u) = mapAdj insertVertex g
    where insertVertex = IM.insert u S.empty

addVertexIfMissing :: Graph w -> Vertex -> Graph w
addVertexIfMissing g (Vertex u) = mapAdj insertVertex g
    where insertVertex a = IM.union a $ IM.singleton u S.empty

degree :: Graph w -> Vertex -> Maybe Int
degree g (Vertex u) = S.size <$> IM.lookup u (adjList g)

hasVertex :: Graph w -> Vertex -> Bool
hasVertex g (Vertex u) = IM.member u (adjList g)

neighbors :: Graph w -> Vertex -> Maybe (Nbors w)
neighbors g (Vertex u) = IM.lookup u (adjList g)

order :: Graph w -> Int
order = IM.size . adjList

vertexSet :: Graph w -> S.Set Vertex
vertexSet = S.fromList . vertices

vertices :: Graph w -> [Vertex]
vertices = fmap Vertex . IM.keys . adjList

-- Edge centric functions
addEdge :: Graph w -> Edge w -> Graph w
addEdge g e = mapAdj insertEdge g
    where oneWay = IM.singleton (value $ src e) (S.singleton e)
          toInsert = if isUndirected g then IM.insert (value $ dest e) (S.singleton (reverse e)) oneWay
                     else oneWay
          insertEdge a = IM.unionWith S.union a toInsert

edges :: Graph w -> S.Set (Edge w)
edges g = S.fromList $ filter predicate $ IM.elems (adjList g) >>= S.toList
    where predicate = if isUndirected g then const True
                      else \e -> let (f, t) = endpoints e in f <= t

getEdge :: Graph w -> (Vertex, Vertex) -> Maybe (Edge w)
getEdge g e@(u, _) = do
    a <- IM.lookup (value u) $ adjList g
    let f = S.filter (\nbor -> endpoints nbor == e) a
    if S.null f then Nothing else Just $ head (S.toList f)

hasEdge :: Graph w -> (Vertex, Vertex) -> Bool
hasEdge g e = Ma.isJust $ getEdge g e

size :: Graph w -> Int
size = S.size . edges

weightOf :: Graph w -> (Vertex, Vertex) -> Maybe w
weightOf g e = weight <$> getEdge g e

-- Graph functions
bfs :: Graph w -> Vertex -> Maybe (DM.Map Vertex Int)
bfs g r = bfsAux g (DM.singleton r 0) (Q.singleton r)

cost :: Mo.Monoid w => Graph w -> [Vertex] -> Maybe w
cost _ [] = Just Mo.mempty
cost g l@[_] = if hasPath g l then Just Mo.mempty else Nothing
cost g vs = if not $ hasPath g vs then Nothing
            else sumWeights <$> mapM (g `getEdge`) (pathToEdges vs)
    where sumWeights = F.fold . fmap weight

directed :: Graph w -> Graph w
directed g@(DGraph _) = g
directed (UGraph a) = DGraph a

hasPath :: Graph w -> [Vertex] -> Bool
hasPath _ [] = True
hasPath g [u] = hasVertex g u
hasPath g vs = length (takeWhile (g `hasEdge`) edgePath) == length edgePath
    where edgePath = pathToEdges vs

isDirected :: Graph w -> Bool
isDirected (DGraph _) = True
isDirected (UGraph _) = False

isUndirected :: Graph w -> Bool
isUndirected (DGraph _) = False
isUndirected (UGraph _) = True

stronglyConnected :: Graph w -> Bool
stronglyConnected g = (length vs <= 1) || Ma.fromMaybe False bfsr
    where vs = vertices g
          bfsr = do r <- Ma.listToMaybe vs
                    b <- bfs g r
                    return $ DM.keysSet b == vertexSet g

undirected :: Graph w -> Graph w
undirected g@(UGraph _) = g
undirected g@(DGraph a) = UGraph $ IM.unionWith S.union a $ IM.fromListWith S.union reversedEdges
     where pairReverse e = (value $ dest e, S.singleton $ reverse e)
           reversedEdges = pairReverse <$> S.toList (edges g)

weaklyConnected :: Graph w -> Bool
weaklyConnected = stronglyConnected . undirected

-- Constructors
emptyD :: [Vertex] -> Graph w
emptyD = foldr (flip addVertex) nullD

emptyU :: [Vertex] -> Graph w
emptyU = foldr (flip addVertex) nullU

fromEdgesD :: [Edge w] -> Graph w
fromEdgesD = foldr (flip addEdge) nullD

fromEdgesU :: [Edge w] -> Graph w
fromEdgesU = foldr (flip addEdge) nullU

nullD :: Graph w
nullD = DGraph IM.empty

nullU :: Graph w
nullU = UGraph IM.empty

-- File IO
readAdjacencyListU :: IO.FilePath -> IO (Maybe (Graph Int))
readAdjacencyListU = fmap (Atto.maybeResult . Atto.parse parseAdjU) . C8.readFile

readAdjacencyListW :: IO.FilePath -> Atto.Parser w -> IO (Maybe (Graph w))
readAdjacencyListW fp p = fmap (Atto.maybeResult . Atto.parse (parseAdjW p)) $ C8.readFile fp

writeAdjacencyListU :: IO.FilePath -> Graph w -> IO ()
writeAdjacencyListU fp g = IO.writeFile fp $ ppAdj ppEdgeU g

writeAdjacencyListW :: Show w => IO.FilePath -> Graph w -> IO ()
writeAdjacencyListW fp g = IO.writeFile fp $ ppAdj ppEdgeW g

writeEdgelistU :: IO.FilePath -> Graph w -> IO ()
writeEdgelistU fp g = IO.writeFile fp $ ppEdgeList ppEdgeU g

writeEdgelistW :: Show w => IO.FilePath -> Graph w -> IO ()
writeEdgelistW fp g = IO.writeFile fp $ ppEdgeList ppEdgeW g

-- Parsers for reading graph from file
parseAdjU :: Atto.Parser (Graph Int)
parseAdjU = (UGraph . IM.fromList) <$> Atto.many' parseAdjLU

parseAdjW :: Atto.Parser w -> Atto.Parser (Graph w)
parseAdjW p = (DGraph . IM.fromList) <$> Atto.many' (parseAdjLW p)

parseAdjLU :: Atto.Parser (Int, Nbors Int)
parseAdjLU = parseAdjLW (return 1)

parseAdjLW :: Atto.Parser w -> Atto.Parser (Int, Nbors w)
parseAdjLW p = do
    u <- parseVertex
    Atto.skipSpace
    vs <- parseNbor p `Atto.sepBy` Atto.skipSpace
    Atto.endOfLine
    let es = mapM (uncurry (edge u)) vs
    maybe A.empty (\es' -> return (value u, S.fromList es')) es

parseNbor :: Atto.Parser w -> Atto.Parser (Vertex, w)
parseNbor p = do
    v <- parseVertex
    Atto.skipSpace
    w <- p
    return (v, w)

parseVertex :: Atto.Parser Vertex
parseVertex = Vertex <$> (Atto.decimal :: Atto.Parser Int)

-- Helper functions
adjList :: Graph w -> AdjList w
adjList (DGraph a) = a
adjList (UGraph a) = a

bfsAux :: Graph w -> DM.Map Vertex Int -> Q.Queue Vertex -> Maybe (DM.Map Vertex Int)
bfsAux g m q = if not $ Q.null q then recurse else Just m
    where recurse = do (u, rest) <- Q.dequeue q
                       pathLen   <- DM.lookup u m
                       nbors     <- neighbors g u
                       let (m', q') =  bfsStep m rest nbors (pathLen + 1)
                       bfsAux g m' q'

bfsStep :: DM.Map Vertex Int -> Q.Queue Vertex -> Nbors w -> Int -> (DM.Map Vertex Int, Q.Queue Vertex)
bfsStep m q ns x = S.foldr step (m, q) ns
    where step n p@(m', q') = if DM.member (dest n) m then p
                            else (DM.insert (dest n) x m', Q.enqueue (dest n) q')

mapAdj :: (AdjList w -> AdjList w) -> Graph w -> Graph w
mapAdj f (DGraph a) = DGraph $ f a
mapAdj f (UGraph a) = UGraph $ f a

pathToEdges :: [Vertex] -> [(Vertex, Vertex)]
pathToEdges vs = zip vs (drop 1 vs)

ppAdj :: (Edge w -> String) -> Graph w -> String
ppAdj f g = unlines $ fmap pp $ IM.toList $ adjList g
    where pp (u, nbors) = show u ++ " " ++ ppNbor f (S.toList nbors)

ppEdgeList :: (Edge w -> String) -> Graph w -> String
ppEdgeList f g = unlines $ fmap f $ S.toList $ edges g

ppEdgeU :: Edge w -> String
ppEdgeU e = show (src e) ++ " " ++ show (dest e)

ppEdgeW :: Show w => Edge w -> String
ppEdgeW e = show (src e) ++ " " ++ show (dest e) ++ show (weight e)

ppNbor :: (Edge w -> String) -> [Edge w] -> String
ppNbor f es = unwords $ fmap f es
