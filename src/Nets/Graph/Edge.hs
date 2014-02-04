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

data Edge w = Edge Int Int w

edge :: Int -> Int -> w -> Edge w
edge = Edge

from :: Edge w -> Int
from (Edge f _ _) = f

to :: Edge w -> Int
to (Edge _ t _) = t

weight :: Edge w -> w
weight (Edge _ _ w) = w

endpoints :: Edge w -> (Int, Int)
endpoints (Edge f t _) = (f, t)

reverse :: Edge w -> Edge w
reverse (Edge f t w) = Edge t f w

-- Edges are compared only on endpoints
instance Eq (Edge w) where
    (==) e1 e2 = (==) (endpoints e1) (endpoints e2)

instance Ord (Edge w) where
    compare e1 e2 = compare (endpoints e1) (endpoints e2)
