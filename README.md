# nets (Haskell)
Nets is a graph/complex network analysis library for Haskell.
The library design is based primarily on my prior research
experience on large-scale graph analysis in the context of
online social networks, synthetic graph generation, and
random walks.

## Design
* The data structure backing the graph is an adjacency list,
  as many of the graphs I work with are sparse.
    * The adjacency list is Haskell's `Data.IntMap`, a radix 
      tree.
* The "list" part of the adjacency list is `Data.Set`, a
  balanced binary tree. I tend to need a lot of random
  neighbor selection, so the logarithmic complexity leaves
  a bit to be desired. I am looking into alternatives.
* The `Ord` instance for `Nets.Graph.Edge` only considers
  endpoints, not the weight. Seems a bit hack-y, I don't
  really like it.. need to think of a better way.

## License
Please see LICENSE for licensing details.
