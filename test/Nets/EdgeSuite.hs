module Nets.EdgeSuite (tests) where

import qualified Data.Foldable as F

import Test.Framework

import Test.Framework.Providers.HUnit
import qualified Test.HUnit.Base as HU

import Test.Framework.Providers.QuickCheck2
import qualified Test.QuickCheck as QC

import qualified Nets.Edge as E
import Nets.Vertex

tests :: [Test]
tests = 
    [
        testGroup "Edge suite"
        [
            testGroup "Edges cannot have loops" $ hUnitTestToTests noLoop,
            testProperty "Endpoints of an edge is the same as the tuple used to create it" endpointId
        ]
    ]

noLoop :: HU.Test
noLoop =
    "Edges cannot have loops" HU.~: E.unweighted (Vertex 1) (Vertex 1) HU.@=? Nothing

endpointId :: (Int, Int) -> QC.Property
endpointId (u, v) = u /= v QC.==> F.all (\e -> E.endpoints e == (Vertex u, Vertex v)) (E.unweighted (Vertex u) (Vertex v))
