{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Toolbox.Worker
  ( moduleGraphWorker,
    tempWorker,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Data.Bifunctor (second)
import qualified Data.Foldable as F
import Data.Function (on)
import Data.Functor.Identity (runIdentity)
import Data.Graph (Graph, buildG, topSort)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Maybe (mapMaybe)
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
import Toolbox.Util.Graph.BFS (runMultiseedStagedBFS)
import Toolbox.Util.Graph.Builder
  ( makeBiDep,
    makeEdges,
    makeRevDep,
  )
import Toolbox.Util.Graph.Cluster
  ( ClusterState (..),
    ClusterVertex (..),
    filterOutSmallNodes,
    fullStep,
    makeDivisionsInOrder,
    makeSeedState,
    reduceGraphByPath,
  )

moduleGraphWorker :: TVar ServerState -> ModuleGraphInfo -> IO ()
moduleGraphWorker var mgi = do
  grVisInfo <- layOutGraph modNameMap reducedGraphReversed
  putStrLn "########"
  F.for_ divisions $ \(i, js) ->
    F.for_ (IM.lookup i modNameMap) $ \clusterName ->
      print (clusterName, length js)
  putStrLn "********"
  F.for_ clustering $ \(c, js) ->
    print (c, length js)
  putStrLn "%%%%%%%%"
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
  where
    modNameMap = mginfoModuleNameMap mgi
    modDep = mginfoModuleDep mgi
    modRevDep = makeRevDep modDep
    modBiDep = makeBiDep modDep
    nVtx = F.length $ mginfoModuleNameMap mgi
    -- separate large/small nodes
    allNodes = IM.keys modNameMap
    largeNodes = filterOutSmallNodes modDep
    smallNodes = allNodes L.\\ largeNodes
    -- compute reduced graph
    es = makeEdges modDep
    g = buildG (1, nVtx) es
    tordVtxs = reverse $ mginfoModuleTopSorted mgi
    tordSeeds = filter (`elem` largeNodes) tordVtxs
    reducedGraph = reduceGraphByPath g tordSeeds
    reducedGraphReversed = makeRevDep reducedGraph
    -- compute clustering
    -- as we use modRevDep as the graph, the greediness has precedence towards upper dependencies.
    divisions = makeDivisionsInOrder tordVtxs tordSeeds
    seedsWithWhiteList = fmap (second Just) divisions
    modUndirDep = fmap (\(outs, ins) -> outs ++ ins) $ modBiDep
    bfsResult =
      runIdentity $
        runMultiseedStagedBFS
          (\_ -> pure ())
          modUndirDep
          seedsWithWhiteList
    clustering = mapMaybe convert bfsResult
      where
        convert (i, stages) = do
          let js = concat stages
          clusterName <- IM.lookup i modNameMap
          let members = mapMaybe (\j -> (j,) <$> IM.lookup j modNameMap) js
          pure (clusterName, members)

maxSubGraphSize :: Int
maxSubGraphSize = 30 -- 1000 -- 100 -- 30

layOutModuleSubgraph ::
  ModuleGraphInfo ->
  (ModuleName, [(Int, ModuleName)]) ->
  IO (ModuleName, GraphVisInfo)
layOutModuleSubgraph mgi (clusterName, members_) = do
  let members = fmap fst members_
      modNameMap = mginfoModuleNameMap mgi
      modDep = mginfoModuleDep mgi
      modBiDep = makeBiDep modDep
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
      subModDepReversed = makeRevDep subModDep
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

tempWorker :: ModuleGraphInfo -> IO ()
tempWorker mgi = do
  let seeds = filterOutSmallNodes (mginfoModuleDep mgi)
      modNameMap = mginfoModuleNameMap mgi
      modDep = mginfoModuleDep mgi
      largeNodes = filterOutSmallNodes modDep
      modRevDep = makeRevDep modDep

  r1 <- runMultiseedStagedBFS (\_ -> pure ()) (IM.fromList testGraph) [(2, Just [2, 4, 5, 7, 9]), (9, Just [1, 3, 6, 7, 8, 9, 10])]
  print r1
  -- mapM_ (\(i, jss) -> print (i, length (concat jss))) r1

  r2 <- runMultiseedStagedBFS (\_ -> pure ()) modRevDep (fmap (,Nothing) largeNodes)
  mapM_ (\(i, jss) -> print (i, length (concat jss))) r2

  let modNameMap = mginfoModuleNameMap mgi
      nVtx = F.length modNameMap
      gr = buildG (1, nVtx) (makeEdges modDep)
      sorted = topSort gr
      seedsInOrder = filter (`elem` seeds) sorted

      divs = makeDivisionsInOrder sorted seedsInOrder

  putStrLn "########"
  F.for_ divs $ \(i, js) ->
    F.for_ (IM.lookup i modNameMap) $ \clusterName ->
      print (clusterName, length js)
