module GHCSpecter.GraphLayout.Sugiyama
  ( layOutGraph,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (allocate)
import Data.Bits ((.|.))
import Data.Foldable qualified as F
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Tuple (swap)
import GHCSpecter.Channel (ModuleName)
import GHCSpecter.GraphLayout.OGDF
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
import GHCSpecter.GraphLayout.Types
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
