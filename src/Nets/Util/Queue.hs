module Nets.Util.Queue
    (
        Queue,
        dequeue,
        empty,
        enqueue,
        length,
        null,
        singleton,
    ) where

import Prelude hiding (length, null)
import qualified Prelude as P

data Queue a = Queue [a] [a]

empty :: Queue a
empty = Queue [] []

length :: Queue a -> Int
length (Queue l r) = P.length l + P.length r

null :: Queue a -> Bool
null (Queue l r) = P.null l && P.null r

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue l r) = if P.null l then second else first
    where first = fmap (\(x, xs) -> (x, Queue xs r)) $ uncons l
          second = fmap (\(x, xs) -> (x, Queue xs [])) $ uncons (reverse r)

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue l r) = Queue l (x:r)

singleton :: a -> Queue a
singleton x = Queue [x] []

-- Helper functions
uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)