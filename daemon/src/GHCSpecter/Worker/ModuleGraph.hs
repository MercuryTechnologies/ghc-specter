{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module GHCSpecter.Worker.ModuleGraph
  ( moduleGraphWorker,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Lens ((.~))
import Data.Bifunctor (second)
import Data.Foldable qualified as F
import Data.Function (on)
import Data.Functor.Identity (runIdentity)
import Data.Graph (buildG)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Tree (Forest)
import GHCSpecter.Channel
  ( ModuleGraphInfo (..),
    ModuleName,
  )
import GHCSpecter.Render.ModuleGraph (layOutGraph)
import GHCSpecter.Server.Types
  ( GraphVisInfo,
    HasModuleGraphState (..),
    HasServerState (..),
    ServerState (..),
    incrementSN,
  )
import GHCSpecter.UI.Types.Event (DetailLevel (..))
import GHCSpecter.Util.Graph.BFS (runMultiseedStagedBFS)
import GHCSpecter.Util.Graph.Builder
  ( makeBiDep,
    makeEdges,
    makeRevDep,
  )
import GHCSpecter.Util.Graph.Cluster
  ( filterOutSmallNodes,
    makeDivisionsInOrder,
    reduceGraphByPath,
  )
import GHCSpecter.Util.SourceTree (makeSourceTree)
import Text.Printf (printf)

maxSubGraphSize :: DetailLevel -> Int
maxSubGraphSize UpTo30 = 30
maxSubGraphSize UpTo100 = 100
maxSubGraphSize UpTo300 = 300

moduleGraphWorker :: TVar ServerState -> ModuleGraphInfo -> IO ()
moduleGraphWorker var mgi = do
  let forest = makeSourceTree mgi
  grVisInfo <- layOutGraph modNameMap reducedGraphReversed
  atomically $
    modifyTVar' var $
      let updater =
            (serverModuleGraphState . mgsModuleForest .~ forest)
              . (serverModuleGraphState . mgsClusterGraph .~ Just grVisInfo)
              . ( serverModuleGraphState . mgsClustering
                    .~ fmap (\(c, ms) -> (c, fmap snd ms)) clustering
                )
       in incrementSN . updater
  subgraphs <-
    traverse
      (\level -> (level,) <$> traverse (layOutModuleSubgraph mgi level) clustering)
      [UpTo30, UpTo100, UpTo300]
  atomically $
    modifyTVar' var $
      incrementSN . (serverModuleGraphState . mgsSubgraph .~ subgraphs)
  where
    modNameMap = mginfoModuleNameMap mgi
    modDep = mginfoModuleDep mgi
    modBiDep = makeBiDep modDep
    nVtx = F.length $ mginfoModuleNameMap mgi
    -- separate large/small nodes
    largeNodes = filterOutSmallNodes modDep
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

layOutModuleSubgraph ::
  ModuleGraphInfo ->
  DetailLevel ->
  (ModuleName, [(Int, ModuleName)]) ->
  IO (ModuleName, GraphVisInfo)
layOutModuleSubgraph mgi detailLevel (clusterName, members_) = do
  let members = fmap fst members_
      modNameMap = mginfoModuleNameMap mgi
      modDep = mginfoModuleDep mgi
      modBiDep = makeBiDep modDep
      largeNodes =
        take (maxSubGraphSize detailLevel)
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
  printf "Cluster %s subgraph layout has been calculated.\n" (T.unpack clusterName)
  pure (clusterName, grVisInfo)
