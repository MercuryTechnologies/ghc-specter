module GHCSpecter.Layouter.Graph.Sugiyama
  ( layOutGraph,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.|.))
import Data.Foldable qualified as F
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
import GHCSpecter.Layouter.Graph.OGDF
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
import GHCSpecter.Layouter.Graph.Types
  ( EdgeLayout (..),
    GraphVisInfo (..),
    NodeLayout (..),
    transposeGraphVis,
  )
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

newGA :: Graph -> IO GraphAttributes
newGA g = newGraphAttributes g (nodeGraphics .|. edgeGraphics .|. nodeLabel .|. nodeStyle)

layOutGraph :: IntMap Text -> IntMap [Int] -> IO GraphVisInfo
layOutGraph nameMap graph = runGraphLayouter $ do
  g <- liftIO newGraph
  ga <- liftIO $ newGA g

  nodeMap <-
    flip IM.traverseMaybeWithKey graph $ \i _ -> do
      case IM.lookup i nameMap of
        Nothing -> pure Nothing
        Just name -> do
          let h = 4 * T.length name
          node <- newGraphNodeWithSize (g, ga) (15, h)
          appendText ga node name
          pure (Just node)
  nodeIndex <-
    traverse (\node -> liftIO (fromIntegral @_ @Int <$> nodeElement_index node)) nodeMap
  let nodeRevIndex = IM.fromList $ fmap swap $ IM.toList nodeIndex
  void $
    flip IM.traverseWithKey graph $ \i js ->
      F.for_ (IM.lookup i nodeMap) $ \node_i -> do
        let node_js = mapMaybe (\j -> IM.lookup j nodeMap) js
        F.for_ node_js $ \node_j ->
          liftIO (graph_newEdge g node_i node_j)

  doSugiyamaLayout ga

  canvasDim <- getCanvasDim ga

  nodLayout0 <- getAllNodeLayout g ga
  let nodLayout = mapMaybe replace nodLayout0
        where
          replace (NodeLayout j pt dim) = do
            i <- IM.lookup j nodeRevIndex
            name <- IM.lookup i nameMap
            pure $ NodeLayout (i, name) pt dim

  edgLayout0 <- getAllEdgeLayout g ga
  let edgLayout = mapMaybe replace edgLayout0
        where
          replace (EdgeLayout k (start, end) srcTgtPts vertices) = do
            startIdx <- IM.lookup start nodeRevIndex
            endIdx <- IM.lookup end nodeRevIndex
            pure (EdgeLayout k (startIdx, endIdx) srcTgtPts vertices)

  liftIO $ delete ga
  liftIO $ delete g
  let gvisInfo0 = GraphVisInfo canvasDim nodLayout edgLayout
  pure $ transposeGraphVis gvisInfo0
