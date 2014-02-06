module Nets.Graph.Edge
    (
        Edge,
        edge,
        from,
        to,
        weight,
        endpoints,
        reverse
    ) where

import Prelude hiding (reverse)

import Nets.Graph.Vertex

data Edge w = Edge Vertex Vertex w

edge :: Vertex -> Vertex -> w -> Edge w
edge = Edge

from :: Edge w -> Vertex
from (Edge f _ _) = f

to :: Edge w -> Vertex
to (Edge _ t _) = t

weight :: Edge w -> w
weight (Edge _ _ w) = w

endpoints :: Edge w -> (Vertex, Vertex)
endpoints (Edge f t _) = (f, t)

reverse :: Edge w -> Edge w
reverse (Edge f t w) = Edge t f w

-- Edges are compared only on endpoints
instance Eq (Edge w) where
    (==) e1 e2 = (==) (endpoints e1) (endpoints e2)

instance Ord (Edge w) where
    compare e1 e2 = compare (endpoints e1) (endpoints e2)
