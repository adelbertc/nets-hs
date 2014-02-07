module Nets.Edge
    (
        Edge,
        dest,
        edge,
        endpoints,
        reverse,
        src,
        unweighted,
        weight
    ) where

import Prelude hiding (reverse)

import Nets.Vertex

data Edge w = Edge Vertex Vertex w deriving (Show)

dest :: Edge w -> Vertex
dest (Edge _ d _) = d

edge :: Vertex -> Vertex -> w -> Maybe (Edge w)
edge u v w = if u == v then Nothing else Just $ Edge u v w

endpoints :: Edge w -> (Vertex, Vertex)
endpoints (Edge s d _) = (s, d)

reverse :: Edge w -> Edge w
reverse (Edge s d w) = Edge d s w

src :: Edge w -> Vertex
src (Edge s _ _) = s

unweighted :: Vertex -> Vertex -> Maybe (Edge Int)
unweighted u v = if u == v then Nothing else Just $ Edge u v 1

weight :: Edge w -> w
weight (Edge _ _ w) = w

-- Edges are compared only on endpoints
instance Eq (Edge w) where
    (==) e1 e2 = (==) (endpoints e1) (endpoints e2)

instance Ord (Edge w) where
    compare e1 e2 = compare (endpoints e1) (endpoints e2)
