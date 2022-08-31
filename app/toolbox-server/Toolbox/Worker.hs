{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Toolbox.Worker
  ( moduleGraphWorker,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import qualified Data.Foldable as F
import Data.Function (on)
import Data.Graph (buildG)
import Data.IntMap (IntMap)
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
    makeReducedGraph,
    makeSeedState,
  )

makeReducedGraphReversedFromModuleGraph :: ModuleGraphInfo -> IntMap [Int]
makeReducedGraphReversedFromModuleGraph mgi =
  let modDep = mginfoModuleDep mgi
      nVtx = F.length $ mginfoModuleNameMap mgi
      es = makeEdges modDep
      g = buildG (1, nVtx) es
      seeds = filterOutSmallNodes modDep
      tordVtxs = reverse $ mginfoModuleTopSorted mgi
      tordSeeds = filter (`elem` seeds) tordVtxs
      reducedGraph = makeReducedGraph g tordSeeds
   in makeRevDep reducedGraph

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
      bgr = makeBiDep (mginfoModuleDep mgi)
      -- run clustering twice
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
