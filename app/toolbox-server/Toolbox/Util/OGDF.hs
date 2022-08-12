{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Toolbox.Util.OGDF
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
    newGraphNodeWithSize,
    appendText,
    setFillColor,
    getNodeX,
    getNodeY,
    getNodeWidth,
    getNodeHeight,
    --
    getAllNodeLayout,
    getAllEdgeLayout,
  )
where

import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Extra (ifM, loopM)
import Data.ByteString (useAsCString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import FFICXX.Runtime.TH (IsCPrimitive (..), TemplateParamInfo (..))
import Foreign.C.Types (CBool (..), CLong)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable (peek, poke))
import OGDF.Color (color_fromString)
import OGDF.DPoint
import OGDF.DPoint.Implementation (dPoint_m_x_get, dPoint_m_y_get)
import OGDF.EdgeElement
  ( EdgeElement (..),
    edgeElement_index,
    edgeElement_succ,
  )
import OGDF.Graph
  ( Graph,
    graph_firstEdge,
    graph_firstNode,
    graph_newNode,
  )
import OGDF.GraphAttributes
  ( GraphAttributes,
    graphAttributes_bends,
    graphAttributes_fillColor,
    graphAttributes_height,
    graphAttributes_label,
    graphAttributes_width,
    graphAttributes_x,
    graphAttributes_y,
  )
import qualified OGDF.List.TH as TH
import OGDF.List.Template
import qualified OGDF.ListIterator.TH as TH
import OGDF.ListIterator.Template
import OGDF.NodeElement
  ( NodeElement (..),
    nodeElement_index,
    nodeElement_succ,
  )
import STD.CppString (cppString_append, newCppString)
import STD.Deletable (delete)

TH.genListInstanceFor
  NonCPrim
  ( [t|DPoint|]
  , TPInfo
      { tpinfoCxxType = "DPoint"
      , tpinfoCxxHeaders = ["ogdf/basic/geometry.h", "OGDFType.h"]
      , tpinfoCxxNamespaces = ["ogdf"]
      , tpinfoSuffix = "DPoint"
      }
  )

TH.genListIteratorInstanceFor
  NonCPrim
  ( [t|DPoint|]
  , TPInfo
      { tpinfoCxxType = "DPoint"
      , tpinfoCxxHeaders = ["ogdf/basic/geometry.h", "OGDFType.h"]
      , tpinfoCxxNamespaces = ["ogdf"]
      , tpinfoSuffix = "DPoint"
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

newGraphNodeWithSize :: (Graph, GraphAttributes) -> (Int, Int) -> IO NodeElement
newGraphNodeWithSize (g, ga) (width, height) = do
  node <- graph_newNode g
  p_width <- graphAttributes_width ga node
  poke p_width (fromIntegral width)
  p_height <- graphAttributes_height ga node
  poke p_height (fromIntegral height)
  pure node

appendText :: GraphAttributes -> NodeElement -> Text -> IO ()
appendText ga node txt = do
  str <- graphAttributes_label ga node
  let bs = encodeUtf8 txt
  useAsCString bs $ \cstr ->
    bracket (newCppString cstr) delete $ \str' ->
      void $ cppString_append str str'

setFillColor :: GraphAttributes -> NodeElement -> Text -> IO ()
setFillColor ga node colorTxt = do
  color <- graphAttributes_fillColor ga node
  let bs = encodeUtf8 colorTxt
  useAsCString bs $ \cstr ->
    bracket (newCppString cstr) delete $ \str' ->
      void $ color_fromString color str'

getNodeX :: GraphAttributes -> NodeElement -> IO Double
getNodeX ga n = realToFrac <$> (peek =<< graphAttributes_x ga n)

getNodeY :: GraphAttributes -> NodeElement -> IO Double
getNodeY ga n = realToFrac <$> (peek =<< graphAttributes_y ga n)

getNodeWidth :: GraphAttributes -> NodeElement -> IO Double
getNodeWidth ga n = realToFrac <$> (peek =<< graphAttributes_width ga n)

getNodeHeight :: GraphAttributes -> NodeElement -> IO Double
getNodeHeight ga n = realToFrac <$> (peek =<< graphAttributes_height ga n)

getAllNodeLayout :: Graph -> GraphAttributes -> IO [(Int, Double, Double, Double, Double)]
getAllNodeLayout g ga = do
  n0 <- graph_firstNode g
  flip loopM ([], n0) $ \(acc, n@(NodeElement nPtr)) ->
    if nPtr == nullPtr
      then pure (Right acc)
      else do
        j <- fromIntegral <$> nodeElement_index n
        x <- getNodeX ga n
        y <- getNodeY ga n
        w <- getNodeWidth ga n
        h <- getNodeHeight ga n
        let acc' = acc ++ [(j, x, y, w, h)]
        Left . (acc',) <$> nodeElement_succ n

getAllEdgeLayout :: Graph -> GraphAttributes -> IO [(Int, [(Double, Double)])]
getAllEdgeLayout g ga = do
  e0 <- graph_firstEdge g
  flip loopM ([], e0) $ \((!acc), e@(EdgeElement ePtr)) ->
    if ePtr == nullPtr
      then pure (Right acc)
      else do
        j :: Int <- fromIntegral <$> edgeElement_index e
        dpline <- graphAttributes_bends ga e
        it0 <- begin dpline
        bendPoints <-
          flip loopM ([], it0) $ \((!bpts), it) -> do
            ifM
              ((/= 0) <$> valid it)
              ( do
                  p <- deRef it
                  x <- realToFrac <$> dPoint_m_x_get p
                  y <- realToFrac <$> dPoint_m_y_get p
                  let bpts' = bpts ++ [(x, y)]
                  (Left . (bpts',) <$> listIteratorSucc it)
              )
              (pure $ Right bpts)
        let acc' = acc ++ [(j, bendPoints)]
        Left . (acc',) <$> edgeElement_succ e
