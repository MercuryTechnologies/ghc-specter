{-# LANGUAGE BangPatterns #-}

module Toolbox.Render.ModuleGraph
  ( layOutGraph,
    filterOutSmallNodes,
    makeReducedGraphReversedFromModuleGraph,
    renderModuleGraph,
  )
where

import Concur.Core (Widget)
import Concur.Replica (div, pre, text)
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Extra (loop)
import Data.Bits ((.|.))
import Data.Foldable (for_)
import qualified Data.Foldable as F
import Data.Graph (buildG, topSort)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)
import OGDF.DRect (dRect_height, dRect_width)
import OGDF.Graph
  ( Graph,
    graph_newEdge,
    newGraph,
  )
import OGDF.GraphAttributes
  ( GraphAttributes,
    boundingBox,
    newGraphAttributes,
  )
import OGDF.LayoutModule (ILayoutModule (call))
import OGDF.MedianHeuristic (newMedianHeuristic)
import OGDF.NodeElement (nodeElement_index)
import OGDF.OptimalHierarchyLayout
  ( newOptimalHierarchyLayout,
    optimalHierarchyLayout_layerDistance,
    optimalHierarchyLayout_nodeDistance,
    optimalHierarchyLayout_weightBalancing,
  )
import OGDF.OptimalRanking (newOptimalRanking)
import OGDF.SugiyamaLayout
  ( newSugiyamaLayout,
    sugiyamaLayout_setCrossMin,
    sugiyamaLayout_setLayout,
    sugiyamaLayout_setRanking,
  )
import Replica.VDOM.Types (HTML)
import STD.Deletable (delete)
import Toolbox.Channel
  ( ModuleGraphInfo (..),
    ModuleName,
    SessionInfo (..),
  )
import Toolbox.Server.Types
  ( GraphVisInfo (..),
    ServerState (..),
  )
import Toolbox.Util.Graph
  ( filterOutSmallNodes,
    makeEdges,
    makeReducedGraph,
    mkRevDep,
  )
import Toolbox.Util.OGDF
  ( appendText,
    edgeGraphics,
    getAllEdgeLayout,
    getAllNodeLayout,
    newGraphNodeWithSize,
    nodeGraphics,
    nodeLabel,
    nodeStyle,
  )
import Prelude hiding (div)

analyze :: ModuleGraphInfo -> Text
analyze graphInfo =
  let modDep = mginfoModuleDep graphInfo
      modRevDep = mkRevDep modDep
      initials = IM.keys $ IM.filter (\js -> null js) modDep
      terminals = IM.keys $ IM.filter (\js -> null js) modRevDep
      orphans = initials `L.intersect` terminals
      singles = IM.mapMaybe (\js -> case js of j : [] -> Just j; _ -> Nothing) modDep
      leg i = loop go ([i], i)
        where
          go (acc', i') =
            case IM.lookup i' singles of
              Nothing -> Right acc'
              Just j' -> Left (acc' ++ [j'], j')
      legs = fmap leg (initials L.\\ orphans)
      larges = filterOutSmallNodes graphInfo
      largeNames = mapMaybe (\i -> IM.lookup i (mginfoModuleNameMap graphInfo)) larges
   in "intials: " <> (T.pack $ show initials) <> ",\n"
        <> "terminals: "
        <> (T.pack $ show terminals)
        <> ",\n"
        <> "orphans: "
        <> (T.pack $ show orphans)
        <> ",\n"
        <> "singles: "
        <> (T.pack $ show singles)
        <> ",\n"
        <> "legs: "
        <> (T.pack $ show legs)
        <> "\n=============\n"
        <> "larges: "
        <> (T.pack $ show largeNames)
        <> "# of larges: "
        <> (T.pack $ show (length larges))

-- | (number of vertices, number of edges)
stat :: ModuleGraphInfo -> (Int, Int)
stat mgi =
  let nVtx = F.length $ mginfoModuleNameMap mgi
      nEdg = F.sum $ fmap length $ mginfoModuleDep mgi
   in (nVtx, nEdg)

formatModuleGraphInfo :: ModuleGraphInfo -> Text
formatModuleGraphInfo mgi =
  let txt1 =
        T.intercalate "\n" . fmap (T.pack . show) $ IM.toList $ mginfoModuleNameMap mgi
      txt2 =
        T.intercalate "\n" . fmap (T.pack . show) $ IM.toList $ mginfoModuleDep mgi
      txt3 =
        T.pack . show $ mginfoModuleTopSorted mgi
      (nVtx, nEdg) = stat mgi
   in "(key, module):\n"
        <> txt1
        <> "\n-----------------\n"
        <> "dependencies:\n"
        <> txt2
        <> "\n-----------------\n"
        <> "top sorted:\n"
        <> txt3
        <> "\n=================\n"
        <> analyze mgi
        <> "\n=================\n"
        <> "# of vertices: "
        <> T.pack (show nVtx)
        <> ", # of edges: "
        <> T.pack (show nEdg)

renderModuleGraph :: ServerState -> Widget HTML a
renderModuleGraph ss =
  let sessionInfo = serverSessionInfo ss
   in case sessionStartTime sessionInfo of
        Nothing ->
          pre [] [text "GHC Session has not been started"]
        Just _ ->
          div
            []
            [ pre [] [text $ formatModuleGraphInfo (sessionModuleGraph sessionInfo)]
            ]

newGA :: Graph -> IO GraphAttributes
newGA g = newGraphAttributes g (nodeGraphics .|. edgeGraphics .|. nodeLabel .|. nodeStyle)

makeReducedGraphReversedFromModuleGraph :: ModuleGraphInfo -> IntMap [Int]
makeReducedGraphReversedFromModuleGraph mgi =
  let nVtx = F.length $ mginfoModuleNameMap mgi
      es = makeEdges $ mginfoModuleDep mgi
      g = buildG (1, nVtx) es
      seeds = filterOutSmallNodes mgi
      tordVtxs = topSort g
      tordSeeds = filter (`elem` seeds) tordVtxs
      reducedGraph = makeReducedGraph g tordSeeds
   in mkRevDep reducedGraph

layOutGraph :: IntMap ModuleName -> IntMap [Int] -> IO GraphVisInfo
layOutGraph nameMap graph = do
  bracket newGraph delete $ \g ->
    bracket (newGA g) delete $ \ga -> do
      moduleNodeMap <-
        flip IM.traverseMaybeWithKey graph $ \i _ -> do
          case IM.lookup i nameMap of
            Nothing -> pure Nothing
            Just name -> do
              let width = 8 * T.length name
              node <- newGraphNodeWithSize (g, ga) (width, 15)
              appendText ga node name
              pure (Just node)
      moduleNodeIndex <-
        traverse (\node -> fromIntegral @_ @Int <$> nodeElement_index node) moduleNodeMap
      let moduleNodeRevIndex = IM.fromList $ fmap swap $ IM.toList moduleNodeIndex
      void $
        flip IM.traverseWithKey graph $ \i js ->
          for_ (IM.lookup i moduleNodeMap) $ \node_i -> do
            let node_js = mapMaybe (\j -> IM.lookup j moduleNodeMap) js
            for_ node_js $ \node_j ->
              graph_newEdge g node_i node_j

      bracket newSugiyamaLayout delete $ \sl -> do
        orank <- newOptimalRanking
        sugiyamaLayout_setRanking sl orank
        mh <- newMedianHeuristic
        sugiyamaLayout_setCrossMin sl mh
        ohl <- newOptimalHierarchyLayout
        optimalHierarchyLayout_layerDistance ohl 5.0
        optimalHierarchyLayout_nodeDistance ohl 1.0
        optimalHierarchyLayout_weightBalancing ohl 0.5
        sugiyamaLayout_setLayout sl ohl
        call sl ga

        drect <- boundingBox ga
        canvasWidth :: Double <- realToFrac <$> dRect_width drect
        canvasHeight :: Double <- realToFrac <$> dRect_height drect

        nodeLayout0 <- getAllNodeLayout g ga
        let nodeLayout = mapMaybe replace nodeLayout0
              where
                replace (j, x, y, w, h) = do
                  i <- IM.lookup j moduleNodeRevIndex
                  name <- IM.lookup i nameMap
                  pure (name, x, y, w, h)

        edgeLayout <- getAllEdgeLayout g ga
        print edgeLayout

        pure $ GraphVisInfo (canvasWidth, canvasHeight) nodeLayout edgeLayout
