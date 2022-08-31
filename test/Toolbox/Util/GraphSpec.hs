module Toolbox.Util.GraphSpec (spec) where

import Data.IntMap qualified as IM
import Data.List qualified as L
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Toolbox.Channel (ModuleGraphInfo (..))
import Toolbox.Util.Graph
  ( ClusterState (..),
    ClusterVertex (..),
    degreeInvariant,
    filterOutSmallNodes,
    fullStep,
    makeBiDep,
    makeSeedState,
    totalNumberInvariant,
  )

testGraph :: [(Int, [Int])]
testGraph =
  [ (1, [])
  , (2, [1, 4, 5, 6])
  , (3, [6])
  , (4, [])
  , (5, [4, 7, 8])
  , (6, [8])
  , (7, [9, 10])
  , (8, [9])
  , (9, [])
  , (10, [])
  ]

testGraphInfo :: ModuleGraphInfo
testGraphInfo =
  ModuleGraphInfo
    { mginfoModuleNameMap =
        IM.fromList
          [ (1, "A")
          , (2, "B")
          , (3, "C")
          , (4, "D")
          , (5, "E")
          , (6, "F")
          , (7, "G")
          , (8, "H")
          , (9, "I")
          , (10, "J")
          ]
    , mginfoModuleDep = IM.fromList testGraph
    , mginfoModuleTopSorted = []
    }

spec :: Spec
spec =
  describe "Toolbox.Util.Graph greedy downward clustering" $ do
    let bgr = makeBiDep $ mginfoModuleDep testGraphInfo
        allNodes = IM.keys $ mginfoModuleNameMap testGraphInfo
        nNodes = length allNodes
        largeNodes = filterOutSmallNodes testGraphInfo
        smallNodes = allNodes L.\\ largeNodes
        seeds =
          ClusterState
            { clusterStateClustered =
                fmap (\i -> (Cluster i, [i])) largeNodes
            , clusterStateUnclustered = smallNodes
            }
        clustering0 = seeds
        graph0 = makeSeedState largeNodes bgr
        r0 = (clustering0, graph0)
        r1@(clustering1, graph1) = fullStep r0
        _r2@(clustering2, graph2) = fullStep r1

    it "should preserve the number of all the nodes" $ do
      totalNumberInvariant clustering0 `shouldBe` nNodes
      totalNumberInvariant clustering1 `shouldBe` nNodes
      totalNumberInvariant clustering2 `shouldBe` nNodes

    it "should keep in-degree being matched with out-degree of the derived bi-dep graph after clustering" $ do
      let (outdeg0, indeg0) = degreeInvariant graph0
          (outdeg1, indeg1) = degreeInvariant graph1
          (outdeg2, indeg2) = degreeInvariant graph2
      outdeg0 `shouldBe` indeg0
      outdeg1 `shouldBe` indeg1
      outdeg2 `shouldBe` indeg2
