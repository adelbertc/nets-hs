module Nets.Util.QueueSuite (tests) where

import Test.Framework

import Test.Framework.Providers.HUnit
import qualified Test.HUnit.Base as HU

import Test.Framework.Providers.QuickCheck2

import qualified Nets.Util.Queue as Q

tests :: [Test]
tests = 
    [
        testGroup "Queue suite"
        [
            testGroup "Dequeue null is nothing" $ hUnitTestToTests dequeueEmptyIsNothing,
            testProperty "Enqueue-ing n elements results in queue of length n" queueSize
        ]
    ]

dequeueEmptyIsNothing :: HU.Test
dequeueEmptyIsNothing =
    "Dequeue null queue is Nothing" HU.~: Q.dequeue (Q.empty :: Q.Queue Int) HU.@=? Nothing

queueSize :: [Int] -> Bool
queueSize xs = Q.length (enqueueList xs) == length xs

-- Helpers
enqueueList :: [Int] -> Q.Queue Int
enqueueList = foldr Q.enqueue Q.empty
