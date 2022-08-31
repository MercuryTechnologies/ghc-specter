module Toolbox.Util.Graph.BFSSpec (spec) where

import Data.Functor.Identity (runIdentity)
import Data.IntMap qualified as IM
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Toolbox.Util.Graph (makeBiDep)
import Toolbox.Util.Graph.BFS
  ( runMultiseedStagedBFS,
    runStagedBFS,
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

spec :: Spec
spec =
  describe "Toolbox.Util.Graph.BFS greedy BFS clustering" $ do
    let directedGraph = IM.fromList testGraph
        undirectedGraph = fmap (\(outs, ins) -> outs ++ ins) $ makeBiDep directedGraph
    it "should get correct BFS search result for directed graph" $ do
      let result = runIdentity $ runStagedBFS (\_ -> pure ()) directedGraph 5
      result `shouldBe` [[5], [4, 7, 8], [9, 10]]
    it "should get correct BFS search result for undirected graph" $ do
      let result = runIdentity $ runStagedBFS (\_ -> pure ()) undirectedGraph 5
      result `shouldBe` [[5], [4, 7, 8, 2], [9, 10, 6, 1], [3]]
    it "should get correct BFS search result with multiple seed for undirected graph" $ do
      let result = runIdentity $ runMultiseedStagedBFS (\_ -> pure ()) undirectedGraph [2, 9]
      result `shouldBe` [(2, [[2], [1, 4, 5, 6], [3]]), (9, [[9], [8, 7], [10]])]
