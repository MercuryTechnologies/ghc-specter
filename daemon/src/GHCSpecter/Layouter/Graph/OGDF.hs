{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHCSpecter.Layouter.Graph.OGDF
  ( nodeGraphics,
    edgeGraphics,
    edgeIntWeight,
    edgeDoubleWeight,
    edgeLabel,
    nodeLabel,
    edgeType,
    nodeType,
    nodeId,
    edgeArrow,
    edgeStyle,
    nodeStyle,
    nodeTemplate,
    edgeSubGraphs,
    nodeWeight,
    threeD,
    nodeLabelPosition,
    --
    GraphLayouter (..),
    runGraphLayouter,
    --
    newGraphNodeWithSize,
    appendText,
    setFillColor,
    getNodeX,
    getNodeY,
    getNodeWidth,
    getNodeHeight,
    getCanvasDim,
    --
    getAllNodeLayout,
    getAllEdgeLayout,
    --
    doSugiyamaLayout,
  )
where

import Control.Monad.Extra (ifM, loopM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource
  ( MonadResource (..),
    ResourceT,
    allocate,
    release,
    runResourceT,
  )
import Data.ByteString (useAsCString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import FFICXX.Runtime.TH (IsCPrimitive (..), TemplateParamInfo (..))
import Foreign.C.Types (CBool (..), CLong)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable (peek, poke))
import GHCSpecter.Layouter.Graph.Types
  ( Dimension (..),
    EdgeLayout (..),
    NodeLayout (..),
    Point (..),
  )
import OGDF.Color (color_fromString)
import OGDF.DPoint
import OGDF.DPoint.Implementation (dPoint_m_x_get, dPoint_m_y_get)
import OGDF.DRect (dRect_height, dRect_width)
import OGDF.EdgeElement
  ( EdgeElement (..),
    edgeElement_index,
    edgeElement_source,
    edgeElement_succ,
    edgeElement_target,
  )
import OGDF.Graph
  ( Graph,
    graph_firstEdge,
    graph_firstNode,
    graph_newNode,
  )
import OGDF.GraphAttributes
  ( GraphAttributes,
    boundingBox,
    graphAttributes_bends,
    graphAttributes_fillColor,
    graphAttributes_height,
    graphAttributes_label,
    graphAttributes_width,
    graphAttributes_x,
    graphAttributes_y,
  )
import OGDF.LayoutModule (ILayoutModule (call))
import OGDF.List.TH qualified as TH
import OGDF.List.Template
import OGDF.ListIterator.TH qualified as TH
import OGDF.ListIterator.Template
import OGDF.MedianHeuristic (newMedianHeuristic)
import OGDF.NodeElement
  ( NodeElement (..),
    nodeElement_index,
    nodeElement_succ,
  )
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
import STD.CppString (cppString_append, newCppString)
import STD.Deletable (delete)
import UnliftIO (MonadUnliftIO (withRunInIO))

TH.genListInstanceFor
  NonCPrim
  ( [t|DPoint|],
    TPInfo
      { tpinfoCxxType = "DPoint",
        tpinfoCxxHeaders = ["ogdf/basic/geometry.h", "OGDFType.h"],
        tpinfoCxxNamespaces = ["ogdf"],
        tpinfoSuffix = "DPoint"
      }
  )

TH.genListIteratorInstanceFor
  NonCPrim
  ( [t|DPoint|],
    TPInfo
      { tpinfoCxxType = "DPoint",
        tpinfoCxxHeaders = ["ogdf/basic/geometry.h", "OGDFType.h"],
        tpinfoCxxNamespaces = ["ogdf"],
        tpinfoSuffix = "DPoint"
      }
  )

nodeGraphics :: CLong
nodeGraphics = 0x000001

edgeGraphics :: CLong
edgeGraphics = 0x000002

edgeIntWeight :: CLong
edgeIntWeight = 0x000004

edgeDoubleWeight :: CLong
edgeDoubleWeight = 0x000008

edgeLabel :: CLong
edgeLabel = 0x000010

nodeLabel :: CLong
nodeLabel = 0x000020

edgeType :: CLong
edgeType = 0x000040

nodeType :: CLong
nodeType = 0x000080

nodeId :: CLong
nodeId = 0x000100

edgeArrow :: CLong
edgeArrow = 0x000200

edgeStyle :: CLong
edgeStyle = 0x000400

nodeStyle :: CLong
nodeStyle = 0x000800

nodeTemplate :: CLong
nodeTemplate = 0x001000

edgeSubGraphs :: CLong
edgeSubGraphs = 0x002000

nodeWeight :: CLong
nodeWeight = 0x004000

threeD :: CLong
threeD = 0x008000

nodeLabelPosition :: CLong
nodeLabelPosition = 0x010000

newtype GraphLayouter a = GraphLayouter {unGraphLayouter :: ResourceT IO a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadResource, MonadUnliftIO)

runGraphLayouter :: GraphLayouter a -> IO a
runGraphLayouter (GraphLayouter m) = runResourceT m

newGraphNodeWithSize :: (Graph, GraphAttributes) -> (Int, Int) -> GraphLayouter NodeElement
newGraphNodeWithSize (g, ga) (width, height) = liftIO $ do
  node <- graph_newNode g
  p_width <- graphAttributes_width ga node
  poke p_width (fromIntegral width)
  p_height <- graphAttributes_height ga node
  poke p_height (fromIntegral height)
  pure node

appendText :: GraphAttributes -> NodeElement -> Text -> GraphLayouter ()
appendText ga node txt =
  withRunInIO $ \runInIO -> do
    str <- graphAttributes_label ga node
    let bs = encodeUtf8 txt
    useAsCString bs $ \cstr ->
      runInIO $ do
        (k', str') <- allocate (newCppString cstr) delete
        _ <- liftIO $ cppString_append str str'
        release k'

setFillColor :: GraphAttributes -> NodeElement -> Text -> GraphLayouter ()
setFillColor ga node colorTxt =
  withRunInIO $ \runInIO -> do
    color <- graphAttributes_fillColor ga node
    let bs = encodeUtf8 colorTxt
    useAsCString bs $ \cstr ->
      runInIO $ do
        (k', str') <- allocate (newCppString cstr) delete
        _ <- liftIO $ color_fromString color str'
        release k'

getNodeX :: GraphAttributes -> NodeElement -> GraphLayouter Double
getNodeX ga n =
  liftIO $
    realToFrac <$> (peek =<< graphAttributes_x ga n)

getNodeY :: GraphAttributes -> NodeElement -> GraphLayouter Double
getNodeY ga n =
  liftIO $
    realToFrac <$> (peek =<< graphAttributes_y ga n)

getNodeWidth :: GraphAttributes -> NodeElement -> GraphLayouter Double
getNodeWidth ga n =
  liftIO $
    realToFrac <$> (peek =<< graphAttributes_width ga n)

getNodeHeight :: GraphAttributes -> NodeElement -> GraphLayouter Double
getNodeHeight ga n =
  liftIO $
    realToFrac <$> (peek =<< graphAttributes_height ga n)

getCanvasDim :: GraphAttributes -> GraphLayouter Dimension
getCanvasDim ga = liftIO $ do
  drect <- boundingBox ga
  canvasWidth :: Double <- realToFrac <$> dRect_width drect
  canvasHeight :: Double <- realToFrac <$> dRect_height drect
  pure $ Dim canvasWidth canvasHeight

-- | retrieve node layout information with bare node index.
getAllNodeLayout :: Graph -> GraphAttributes -> GraphLayouter [NodeLayout Int]
getAllNodeLayout g ga = do
  n0 <- liftIO $ graph_firstNode g
  flip loopM ([], n0) $ \(acc, n@(NodeElement nPtr)) ->
    if nPtr == nullPtr
      then pure (Right acc)
      else do
        j <- fromIntegral <$> liftIO (nodeElement_index n)
        x <- getNodeX ga n
        y <- getNodeY ga n
        w <- getNodeWidth ga n
        h <- getNodeHeight ga n
        let acc' = acc ++ [NodeLayout j (Point x y) (Dim w h)]
        Left . (acc',) <$> liftIO (nodeElement_succ n)

getAllEdgeLayout :: Graph -> GraphAttributes -> GraphLayouter [EdgeLayout]
getAllEdgeLayout g ga = do
  e0 <- liftIO $ graph_firstEdge g
  flip loopM ([], e0) $ \((!acc), e@(EdgeElement ePtr)) ->
    if ePtr == nullPtr
      then pure (Right acc)
      else do
        j :: Int <- fromIntegral <$> liftIO (edgeElement_index e)
        src <- liftIO (edgeElement_source e)
        isrc <- fromIntegral <$> liftIO (nodeElement_index src)
        srcX <- getNodeX ga src
        srcY <- getNodeY ga src
        tgt <- liftIO (edgeElement_target e)
        itgt <- fromIntegral <$> liftIO (nodeElement_index tgt)
        tgtX <- getNodeX ga tgt
        tgtY <- getNodeY ga tgt
        dpline <- liftIO (graphAttributes_bends ga e)
        it0 <- liftIO (begin dpline)
        bendPoints <-
          flip loopM ([], it0) $ \((!bpts), it) -> do
            ifM
              ((/= 0) <$> liftIO (valid it))
              ( do
                  p <- liftIO (deRef it)
                  x <- realToFrac <$> liftIO (dPoint_m_x_get p)
                  y <- realToFrac <$> liftIO (dPoint_m_y_get p)
                  let bpts' = bpts ++ [Point x y]
                  (Left . (bpts',) <$> liftIO (listIteratorSucc it))
              )
              (pure $ Right bpts)
        let srcPt = Point srcX srcY
            tgtPt = Point tgtX tgtY
            newEdge = EdgeLayout j (isrc, itgt) (srcPt, tgtPt) bendPoints
            acc' = acc ++ [newEdge]
        Left . (acc',) <$> liftIO (edgeElement_succ e)

doSugiyamaLayout :: GraphAttributes -> GraphLayouter ()
doSugiyamaLayout ga = do
  (_, sl) <- allocate newSugiyamaLayout delete
  liftIO $ do
    orank <- newOptimalRanking
    sugiyamaLayout_setRanking sl orank
    mh <- newMedianHeuristic
    sugiyamaLayout_setCrossMin sl mh
    ohl <- newOptimalHierarchyLayout
    optimalHierarchyLayout_layerDistance ohl 1.0
    optimalHierarchyLayout_nodeDistance ohl 0
    optimalHierarchyLayout_weightBalancing ohl 1.0
    sugiyamaLayout_setLayout sl ohl
    call sl ga
