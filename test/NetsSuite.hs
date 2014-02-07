module Main (main) where

import Test.Framework

import qualified Nets.EdgeSuite as Edge
import qualified Nets.GraphSuite as Graph
import qualified Nets.Util.QueueSuite as Queue

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = Edge.tests ++ Graph.tests ++ Queue.tests
