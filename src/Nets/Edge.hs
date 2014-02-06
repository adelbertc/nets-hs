module Nets.Edge
    (
        Edge,
        dest,
        edge,
        endpoints,
        reverse,
        src,
        weight
    ) where

import Prelude hiding (reverse)

import Nets.Vertex

data Edge w = Edge Vertex Vertex w deriving (Show)

dest :: Edge w -> Vertex
dest (Edge _ t _) = t

edge :: Vertex -> Vertex -> w -> Maybe (Edge w)
edge u v w = if u == v then Nothing else Just $ Edge u v w

endpoints :: Edge w -> (Vertex, Vertex)
endpoints (Edge f t _) = (f, t)

reverse :: Edge w -> Edge w
reverse (Edge f t w) = Edge t f w

src :: Edge w -> Vertex
src (Edge f _ _) = f

weight :: Edge w -> w
weight (Edge _ _ w) = w

-- Edges are compared only on endpoints
instance Eq (Edge w) where
    (==) e1 e2 = (==) (endpoints e1) (endpoints e2)

instance Ord (Edge w) where
    compare e1 e2 = compare (endpoints e1) (endpoints e2)
