module Toolbox.Worker
  ( moduleGraphWorker,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Toolbox.Channel (ModuleGraphInfo (..))
import Toolbox.Render.ModuleGraph (layOutGraph)
import Toolbox.Server.Types
  ( ServerState (..),
    incrementSN,
  )
import Toolbox.Util.Graph
  ( ClusterState (..),
    ClusterVertex (..),
    fullStep,
    getBiDepGraph,
    makeReducedGraphReversedFromModuleGraph,
    makeSeedState,
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
      (clustering, _) = fullStep (seedClustering, makeSeedState largeNodes bgr)
      clusteringFinal = mapMaybe convert (clusterStateClustered clustering)
        where
          convert (Cluster i, js) = do
            clusterName <- IM.lookup i modNameMap
            let members = mapMaybe (\j -> IM.lookup j modNameMap) js
            pure (clusterName, members)
  atomically $
    modifyTVar' var $ \ss ->
      incrementSN $
        ss
          { serverModuleGraph = Just grVisInfo
          , serverModuleClustering = clusteringFinal
          }
