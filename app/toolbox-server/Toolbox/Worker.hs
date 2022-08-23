{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Toolbox.Worker
  ( moduleGraphWorker,
    tempWorker,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Monad (join)
import qualified Data.Foldable as F
import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Maybe (mapMaybe, maybeToList)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS (fromList, notMember, union)
import Data.Tree (Tree (..), drawTree)
import PyF (fmt)
import Toolbox.Channel
  ( ModuleGraphInfo (..),
    ModuleName,
  )
import Toolbox.Render.ModuleGraph (layOutGraph)
import Toolbox.Server.Types
  ( GraphVisInfo,
    ServerState (..),
    incrementSN,
  )
import Toolbox.Util.Graph
  ( ClusterState (..),
    ClusterVertex (..),
    filterOutSmallNodes,
    fullStep,
    getBiDepGraph,
    makeReducedGraphReversedFromModuleGraph,
    makeSeedState,
    mkRevDep,
  )

moduleGraphWorker :: TVar ServerState -> ModuleGraphInfo -> IO ()
moduleGraphWorker var mgi = do
  let modNameMap = mginfoModuleNameMap mgi
      reducedGraphReversed =
        makeReducedGraphReversedFromModuleGraph mgi
  grVisInfo <- layOutGraph modNameMap reducedGraphReversed
  let allNodes = IM.keys modNameMap
      largeNodes = IM.keys reducedGraphReversed
      smallNodes = allNodes L.\\ largeNodes
      seedClustering =
        ClusterState
          { clusterStateClustered = fmap (\i -> (Cluster i, [i])) largeNodes
          , clusterStateUnclustered = smallNodes
          }
      bgr = getBiDepGraph mgi
      (clustering_, _) = fullStep . fullStep $ (seedClustering, makeSeedState largeNodes bgr)
      clustering = mapMaybe convert (clusterStateClustered clustering_)
        where
          convert (Cluster i, js) = do
            clusterName <- IM.lookup i modNameMap
            let members = mapMaybe (\j -> (j,) <$> IM.lookup j modNameMap) js
            pure (clusterName, members)
  atomically $
    modifyTVar' var $ \ss ->
      incrementSN $
        ss
          { serverModuleGraph = Just grVisInfo
          , serverModuleClustering = fmap (\(c, ms) -> (c, fmap snd ms)) clustering
          }

  subgraph <-
    traverse (layOutModuleSubgraph mgi) clustering
  atomically $
    modifyTVar' var $ \ss ->
      incrementSN ss {serverModuleSubgraph = subgraph}

maxSubGraphSize :: Int
maxSubGraphSize = 30

layOutModuleSubgraph ::
  ModuleGraphInfo ->
  (ModuleName, [(Int, ModuleName)]) ->
  IO (ModuleName, GraphVisInfo)
layOutModuleSubgraph mgi (clusterName, members_) = do
  let members = fmap fst members_
      modNameMap = mginfoModuleNameMap mgi
      modDep = mginfoModuleDep mgi
      modBiDep = getBiDepGraph mgi
      largeNodes =
        take maxSubGraphSize
          . fmap fst
          . L.sortBy (flip compare `on` (countEdges . snd))
          . filter (\(m, _) -> m `elem` members)
          . IM.toList
          $ modBiDep
        where
          countEdges (os, is) = length os + length is
      subModDep =
        fmap (\ns -> filter (\n -> n `elem` largeNodes) ns) $
          IM.filterWithKey (\m _ -> m `elem` largeNodes) modDep
      subModDepReversed = mkRevDep subModDep
  grVisInfo <- layOutGraph modNameMap subModDepReversed
  putStrLn [fmt|Cluster {clusterName} subgraph layout has been calculated.|]
  pure (clusterName, grVisInfo)

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

testDFSTree :: Tree Int
testDFSTree = Node 2 [Node 4 [], Node 5 [Node 7 [Node 9 [], Node 10 []], Node 8 []], Node 6 []]

stagedBFS :: IntMap [Int] -> Int -> [[Int]]
stagedBFS graph root = go [[root]] [root]
  where
    getUnvisitedChildren :: IntSet -> Int -> [Int]
    getUnvisitedChildren !visited current =
      let children :: [Int]
          children = join $ maybeToList (IM.lookup current graph)
       in filter (`IS.notMember` visited) children

    step :: IntSet -> [Int] -> Int -> [Int]
    step !stagedSet !newStage current =
      let visited = stagedSet `IS.union` (IS.fromList newStage)
          newChildren = getUnvisitedChildren visited current
          newStage' = newStage ++ newChildren
       in newStage'

    go :: [[Int]] -> [Int] -> [[Int]]
    go !staged nexts =
      case nexts of
        [] -> staged
        _ ->
          let stagedSet = IS.fromList (concat staged)
              newStage = L.foldl' (step stagedSet) [] nexts
              staged' = staged ++ [newStage]
           in go staged' newStage

tempWorker :: ModuleGraphInfo -> IO ()
tempWorker mgi = do
  let seeds = filterOutSmallNodes mgi
  -- print seeds
  -- print mgi

  let gr1 = IM.fromList testGraph
      gr2 = mkRevDep gr1

      gr3 = mkRevDep (mginfoModuleDep mgi)
  print (stagedBFS gr1 2)
  print (stagedBFS gr1 5)
  putStrLn (drawTree $ fmap show testDFSTree)

  print (stagedBFS gr3 2)
