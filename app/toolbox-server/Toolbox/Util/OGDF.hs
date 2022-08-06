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
    getX,
    getY,
    getWidth,
    getHeight,
  )
where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.ByteString (useAsCString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Foreign.C.Types (CLong)
import Foreign.Storable (Storable (peek, poke))
import OGDF.Color (color_fromString)
import OGDF.Graph
  ( Graph,
    graph_newNode,
  )
import OGDF.GraphAttributes
  ( GraphAttributes,
    graphAttributes_fillColor,
    graphAttributes_height,
    graphAttributes_label,
    graphAttributes_width,
    graphAttributes_x,
    graphAttributes_y,
  )
import OGDF.NodeElement (NodeElement (..))
import STD.CppString (cppString_append, newCppString)
import STD.Deletable (delete)

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


getX :: GraphAttributes -> NodeElement -> IO Double
getX ga n = realToFrac <$> (peek =<< graphAttributes_x ga n)

getY :: GraphAttributes -> NodeElement -> IO Double
getY ga n = realToFrac <$> (peek =<< graphAttributes_y ga n)

getWidth :: GraphAttributes -> NodeElement -> IO Double
getWidth ga n = realToFrac <$> (peek =<< graphAttributes_width ga n)

getHeight :: GraphAttributes -> NodeElement -> IO Double
getHeight ga n = realToFrac <$> (peek =<< graphAttributes_height ga n)
