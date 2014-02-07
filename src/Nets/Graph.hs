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
        cost,
        directed,
        directedness,
        hasPath,
        isDirected,
        isUndirected,
        stronglyConnected,
        undirected,
        weaklyConnected,
        empty,
        fromEdges,
        null,
        readAdjacencyListU,
        readAdjacencyListW,
        readEdgeListU,
        readEdgeListW,
        writeAdjacencyListU,
        writeAdjacencyListW,
        writeEdgelistU,
        writeEdgelistW
    ) where

import qualified Control.Applicative as A
import qualified Control.Monad.Trans.Maybe as MaT
import Data.Functor ((<$>))
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import qualified Data.Maybe as Ma
import qualified Data.Monoid as Mo
import qualified Data.Set as S
import Prelude hiding (reverse, null)
import qualified System.IO as IO

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Char8 as C8

import Nets.Edge
import Nets.Vertex
import qualified Nets.Util.Queue as Q

type Nbors w = S.Set (Edge w)
type AdjList w = IM.IntMap (Nbors w)
data Graph w = DGraph (AdjList w) | UGraph (AdjList w) deriving (Eq)

data Directedness = Directed | Undirected

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
cost :: Mo.Monoid w => Graph w -> [Vertex] -> Maybe w
cost _ [] = Just Mo.mempty
cost g l@[_] = if hasPath g l then Just Mo.mempty else Nothing
cost g vs = if not $ hasPath g vs then Nothing
            else sumWeights <$> mapM (g `getEdge`) (pathToEdges vs)
    where sumWeights = F.fold . fmap weight

directed :: Graph w -> Graph w
directed g@(DGraph _) = g
directed (UGraph a) = DGraph a

directedness :: Graph w -> Directedness
directedness (DGraph _) = Directed
directedness (UGraph _) = Undirected

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
stronglyConnected g = (length vs <= 1) || Ma.fromMaybe False reached
    where vs = vertices g
          reached = do r <- mapM (g `reachableFrom`) vs
                       let r' = takeWhile (vertexSet g ==) r
                       return $ length r' == order g

undirected :: Graph w -> Graph w
undirected g@(UGraph _) = g
undirected g@(DGraph a) = UGraph $ IM.unionWith S.union a $ IM.fromListWith S.union reversedEdges
     where pairReverse e = (value $ dest e, S.singleton $ reverse e)
           reversedEdges = pairReverse <$> S.toList (edges g)

weaklyConnected :: Graph w -> Bool
weaklyConnected = stronglyConnected . undirected

-- Constructors
empty :: Directedness -> [Vertex] -> Graph w
empty d = foldr (flip addVertex) (null d)

null :: Directedness -> Graph w
null Directed = DGraph IM.empty
null Undirected = UGraph IM.empty

fromEdges :: Directedness -> [Edge w] -> Graph w
fromEdges d = foldr (flip addEdge) (null d)

-- File IO
readAdjacencyListU :: IO.FilePath -> Directedness -> MaT.MaybeT IO (Graph Int)
readAdjacencyListU fp d = parseGraph fp d parseAdjListU

readAdjacencyListW :: IO.FilePath -> Directedness -> Atto.Parser w -> MaT.MaybeT IO (Graph w)
readAdjacencyListW fp d p = parseGraph fp d (parseAdjListW p)

readEdgeListU :: IO.FilePath -> Directedness -> MaT.MaybeT IO (Graph Int)
readEdgeListU fp d = parseGraph fp d parseEdgeListU

readEdgeListW :: IO.FilePath -> Directedness -> Atto.Parser w -> MaT.MaybeT IO (Graph w)
readEdgeListW fp d p = parseGraph fp d (parseEdgeListW p)

writeAdjacencyListU :: IO.FilePath -> Graph w -> IO ()
writeAdjacencyListU fp g = IO.writeFile fp $ ppAdj ppEdgeU g

writeAdjacencyListW :: Show w => IO.FilePath -> Graph w -> IO ()
writeAdjacencyListW fp g = IO.writeFile fp $ ppAdj ppEdgeW g

writeEdgelistU :: IO.FilePath -> Graph w -> IO ()
writeEdgelistU fp g = IO.writeFile fp $ ppEdgeList ppEdgeU g

writeEdgelistW :: Show w => IO.FilePath -> Graph w -> IO ()
writeEdgelistW fp g = IO.writeFile fp $ ppEdgeList ppEdgeW g

-- Parsers for reading graph from file
parseAdjListU :: Atto.Parser (Graph Int)
parseAdjListU = (DGraph . IM.fromList) <$> Atto.many' (parseAdj $ return 1)

parseAdjListW :: Atto.Parser w -> Atto.Parser (Graph w)
parseAdjListW p = (DGraph . IM.fromList) <$> Atto.many' (parseAdj p)

parseAdj :: Atto.Parser w -> Atto.Parser (Int, Nbors w)
parseAdj p = do
    u <- parseVertex
    Atto.skipSpace
    vs <- parseNbor p `Atto.sepBy` Atto.skipSpace
    Atto.endOfLine
    let es = mapM (uncurry (edge u)) vs
    maybe A.empty (\es' -> return (value u, S.fromList es')) es

parseEdge :: Atto.Parser w -> Atto.Parser (Edge w)
parseEdge p = do
    u <- parseVertex
    Atto.skipSpace
    (v, w) <- parseNbor p
    Atto.endOfLine
    let e = edge u v w
    maybe A.empty return e

parseEdgeListU :: Atto.Parser (Graph Int)
parseEdgeListU = parseEdgeListW $ return 1

parseEdgeListW :: Atto.Parser w -> Atto.Parser (Graph w)
parseEdgeListW = fmap (fromEdges Directed) . Atto.many' . parseEdge

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

bfs :: Graph w -> S.Set Vertex -> Q.Queue Vertex -> Maybe (S.Set Vertex)
bfs g s q = if not $ Q.null q then recurse else Just s
    where recurse = do (u, rest) <- Q.dequeue q
                       nbors     <- neighbors g u
                       let (s', q') =  bfsStep s rest nbors
                       bfs g s' q'

bfsStep :: S.Set Vertex -> Q.Queue Vertex -> Nbors w -> (S.Set Vertex, Q.Queue Vertex)
bfsStep s q = S.foldr step (s, q)
    where step n p@(s', q') = if S.member (dest n) s then p
                            else (S.insert (dest n) s', Q.enqueue (dest n) q')

dirDispatch :: Directedness -> Graph w -> Graph w
dirDispatch Directed = id
dirDispatch Undirected = undirected

mapAdj :: (AdjList w -> AdjList w) -> Graph w -> Graph w
mapAdj f (DGraph a) = DGraph $ f a
mapAdj f (UGraph a) = UGraph $ f a

parseGraph :: FilePath -> Directedness -> Atto.Parser (Graph w) -> MaT.MaybeT IO (Graph w)
parseGraph fp d p = dirDispatch d <$> MaT.MaybeT ((Atto.maybeResult . Atto.parse p) <$> C8.readFile fp)

pathToEdges :: [Vertex] -> [(Vertex, Vertex)]
pathToEdges vs = zip vs (drop 1 vs)

ppAdj :: (Edge w -> String) -> Graph w -> String
ppAdj f g = unlines $ fmap pp $ IM.toList $ adjList g
    where pp (u, nbors) = show u ++ " " ++ ppNbor f (S.toList nbors)

ppEdgeList :: (Edge w -> String) -> Graph w -> String
ppEdgeList f g = unlines $ fmap f $ S.toList $ edges g

ppEdgeU :: Edge w -> String
ppEdgeU e = show (value $ src e) ++ " " ++ show (value $ dest e)

ppEdgeW :: Show w => Edge w -> String
ppEdgeW e = show (value $ src e) ++ " " ++ show (value $ dest e) ++ show (weight e)

ppNbor :: (Edge w -> String) -> [Edge w] -> String
ppNbor f es = unwords $ fmap f es

reachableFrom :: Graph w -> Vertex -> Maybe (S.Set Vertex)
reachableFrom g r = bfs g (S.singleton r) (Q.singleton r)
