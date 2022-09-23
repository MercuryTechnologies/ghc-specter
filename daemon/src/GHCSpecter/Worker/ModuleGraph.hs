{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module GHCSpecter.Worker.ModuleGraph
  ( moduleGraphWorker,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Lens ((.~))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (allocate)
import Data.Bifunctor (second)
import Data.Bits ((.|.))
import Data.Foldable qualified as F
import Data.Function (on)
import Data.Functor.Identity (runIdentity)
import Data.Graph (buildG)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Tuple (swap)
import GHCSpecter.Channel
  ( ModuleGraphInfo (..),
    ModuleName,
  )
import GHCSpecter.Server.Types
  ( EdgeLayout (..),
    GraphVisInfo (..),
    HasModuleGraphState (..),
    HasServerState (..),
    NodeLayout (..),
    ServerState (..),
    incrementSN,
    transposeGraphVis,
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
import GHCSpecter.Util.OGDF
  ( appendText,
    doSugiyamaLayout,
    edgeGraphics,
    getAllEdgeLayout,
    getAllNodeLayout,
    getCanvasDim,
    newGraphNodeWithSize,
    nodeGraphics,
    nodeLabel,
    nodeStyle,
    runGraphLayouter,
  )
import GHCSpecter.Util.SourceTree (makeSourceTree)
import OGDF.Graph
  ( Graph,
    graph_newEdge,
    newGraph,
  )
import OGDF.GraphAttributes
  ( GraphAttributes,
    newGraphAttributes,
  )
import OGDF.NodeElement (nodeElement_index)
import STD.Deletable (delete)
import Text.Printf (printf)

maxSubGraphSize :: DetailLevel -> Int
maxSubGraphSize UpTo30 = 30
maxSubGraphSize UpTo100 = 100
maxSubGraphSize UpTo300 = 300

newGA :: Graph -> IO GraphAttributes
newGA g = newGraphAttributes g (nodeGraphics .|. edgeGraphics .|. nodeLabel .|. nodeStyle)

layOutGraph :: IntMap ModuleName -> IntMap [Int] -> IO GraphVisInfo
layOutGraph nameMap graph = runGraphLayouter $ do
  (_, g) <- allocate newGraph delete
  (_, ga) <- allocate (newGA g) delete

  moduleNodeMap <-
    flip IM.traverseMaybeWithKey graph $ \i _ -> do
      case IM.lookup i nameMap of
        Nothing -> pure Nothing
        Just name -> do
          let h = 4 * T.length name
          node <- newGraphNodeWithSize (g, ga) (15, h)
          appendText ga node name
          pure (Just node)
  moduleNodeIndex <-
    traverse (\node -> liftIO (fromIntegral @_ @Int <$> nodeElement_index node)) moduleNodeMap
  let moduleNodeRevIndex = IM.fromList $ fmap swap $ IM.toList moduleNodeIndex
  void $
    flip IM.traverseWithKey graph $ \i js ->
      F.for_ (IM.lookup i moduleNodeMap) $ \node_i -> do
        let node_js = mapMaybe (\j -> IM.lookup j moduleNodeMap) js
        F.for_ node_js $ \node_j ->
          liftIO (graph_newEdge g node_i node_j)

  doSugiyamaLayout ga

  canvasDim <- getCanvasDim ga

  nodLayout0 <- getAllNodeLayout g ga
  let nodLayout = mapMaybe replace nodLayout0
        where
          replace (NodeLayout j pt dim) = do
            i <- IM.lookup j moduleNodeRevIndex
            name <- IM.lookup i nameMap
            pure $ NodeLayout (i, name) pt dim

  edgLayout0 <- getAllEdgeLayout g ga
  let edgLayout = mapMaybe replace edgLayout0
        where
          replace (EdgeLayout k (start, end) srcTgtPts vertices) = do
            startIdx <- IM.lookup start moduleNodeRevIndex
            endIdx <- IM.lookup end moduleNodeRevIndex
            pure (EdgeLayout k (startIdx, endIdx) srcTgtPts vertices)

  let gvisInfo0 = GraphVisInfo canvasDim nodLayout edgLayout
  pure $ transposeGraphVis gvisInfo0

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
