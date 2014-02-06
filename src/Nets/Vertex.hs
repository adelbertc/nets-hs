module Nets.Vertex where

newtype Vertex = Vertex Int deriving (Eq, Ord, Show)

value :: Vertex -> Int
value (Vertex u) = u
