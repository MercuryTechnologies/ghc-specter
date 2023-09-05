{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module GHCSpecter.Worker.ModuleGraph
  ( moduleGraphWorker,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Data.Bifunctor (second)
import Data.Foldable qualified as F
import Data.Function (on)
import Data.Functor.Identity (runIdentity)
import Data.Graph (buildG)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import GHCSpecter.Channel.Common.Types (type ModuleName)
import GHCSpecter.Channel.Outbound.Types
  ( ModuleGraphInfo (..),
  )
import GHCSpecter.Layouter.Graph.Algorithm.BFS (runMultiseedStagedBFS)
import GHCSpecter.Layouter.Graph.Algorithm.Builder
  ( makeBiDep,
    makeEdges,
    makeRevDep,
  )
import GHCSpecter.Layouter.Graph.Algorithm.Cluster
  ( filterOutSmallNodes,
    makeDivisionsInOrder,
    reduceGraphByPath,
  )
import GHCSpecter.Layouter.Graph.Sugiyama qualified as Sugiyama
import GHCSpecter.Layouter.Graph.Types (GraphVisInfo (..))
import GHCSpecter.Server.Types
  ( ModuleGraphState (..),
    ServerState (..),
    incrementSN,
  )
import GHCSpecter.UI.Types.Event (DetailLevel (..))
import GHCSpecter.Util.SourceTree (makeSourceTree)
import Text.Printf (printf)

maxSubGraphSize :: DetailLevel -> Int
maxSubGraphSize UpTo30 = 30
maxSubGraphSize UpTo100 = 100
maxSubGraphSize UpTo300 = 300

-- | Heuristic value for nodeSizeLimit
--   a * x^n = N
calcNodeSizeLimit :: ModuleGraphInfo -> Int
calcNodeSizeLimit mgi =
  let nMod = length (mgi.mginfoModuleNameMap)
      a = 0.0344
      n = 2.27
      x = floor ((fromIntegral nMod / a) ** (1.0 / n) :: Double)
   in if x < 1 then 1 else x

moduleGraphWorker :: TVar ServerState -> ModuleGraphInfo -> IO ()
moduleGraphWorker var mgi = do
  let nodeSizeLimit = calcNodeSizeLimit mgi
      forest = makeSourceTree mgi
      modNameMap = mginfoModuleNameMap mgi
      mod_names = Set.fromList $ F.toList modNameMap
      modDep = mginfoModuleDep mgi
      modBiDep = makeBiDep modDep
      nVtx = F.length $ mginfoModuleNameMap mgi
      -- separate large/small nodes
      largeNodes = filterOutSmallNodes nodeSizeLimit modDep
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
  grVisInfo <- Sugiyama.layOutGraph modNameMap reducedGraphReversed
  atomically $
    modifyTVar' var $
      let updater =
            \ss ->
              let mgs = ss._serverModuleGraphState
                  mgs' =
                    mgs
                      { _mgsModuleNames = mod_names,
                        _mgsModuleForest = forest,
                        _mgsClusterGraph = Just grVisInfo,
                        _mgsClustering = fmap (\(c, ms) -> (c, fmap snd ms)) clustering
                      }
               in ss {_serverModuleGraphState = mgs'}
       in incrementSN . updater
  subgraphs <-
    traverse
      (\level -> (level,) <$> traverse (layOutModuleSubgraph mgi level) clustering)
      [UpTo30, UpTo100, UpTo300]
  atomically $
    modifyTVar' var $
      incrementSN . (\ss -> ss {_serverModuleGraphState = ss._serverModuleGraphState {_mgsSubgraph = subgraphs}})

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
  grVisInfo <- Sugiyama.layOutGraph modNameMap subModDepReversed
  printf "Cluster %s subgraph layout has been calculated.\n" (T.unpack clusterName)
  pure (clusterName, grVisInfo)
