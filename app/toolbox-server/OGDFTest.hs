module OGDFTest (ogdfTest) where

import Control.Exception (bracket)
import Control.Monad (void, when)
import Control.Monad.Extra (loopM)
import Data.Bits ((.|.))
import Data.Foldable (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Foreign.C.String (withCString)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import OGDF.DPoint
import OGDF.DPolyline
import OGDF.EdgeElement
import OGDF.Graph
import OGDF.GraphAttributes
import OGDF.GraphIO
import OGDF.LayoutModule
import OGDF.MedianHeuristic
import OGDF.NodeElement
import OGDF.OptimalHierarchyLayout
import OGDF.OptimalRanking
import OGDF.SugiyamaLayout
import STD.CppString
import STD.Deletable

nodeGraphics = 0x000001

edgeGraphics = 0x000002

edgeIntWeight = 0x000004

edgeDoubleWeight = 0x000008

edgeLabel = 0x000010

nodeLabel = 0x000020

edgeType = 0x000040

nodeType = 0x000080

nodeId = 0x000100

edgeArrow = 0x000200

edgeStyle = 0x000400

nodeStyle = 0x000800

nodeTemplate = 0x001000

edgeSubGraphs = 0x002000

nodeWeight = 0x004000

threeD = 0x010000

len = 11

newGA :: Graph -> IO GraphAttributes
newGA g = newGraphAttributes g (nodeGraphics .|. edgeGraphics .|. nodeLabel)

ogdfTest :: IO ()
ogdfTest = do
  putStrLn "ogdf test"
  bracket newGraph delete $ \g ->
    bracket (newGA g) delete $ \ga -> do
      forM_ [1 .. len - 1] $ \i -> do
        left <- graph_newNode g
        p_width1 <- graphAttributes_width ga left
        poke p_width1 (fromIntegral (10 * (i + 1)))
        p_height1 <- graphAttributes_height ga left
        poke p_height1 15

        bottom <- graph_newNode g
        p_width2 <- graphAttributes_width ga bottom
        poke p_width2 15
        p_height2 <- graphAttributes_height ga bottom
        poke p_height2 (fromIntegral (10 * (len + 1 - i)))

        e <- graph_newEdge g left bottom
        pure ()

      bracket newSugiyamaLayout delete $ \sl -> do
        or <- newOptimalRanking
        sugiyamaLayout_setRanking sl or
        mh <- newMedianHeuristic
        sugiyamaLayout_setCrossMin sl mh
        ohl <- newOptimalHierarchyLayout
        optimalHierarchyLayout_layerDistance ohl 30.0
        optimalHierarchyLayout_nodeDistance ohl 25.0
        optimalHierarchyLayout_weightBalancing ohl 0.8
        sugiyamaLayout_setLayout sl ohl
        call sl ga

      n0@(NodeElement n0') <- graph_firstNode g
      when (n0' /= nullPtr) $
        void $
          flip loopM n0 $ \n@(NodeElement n'') ->
            if n'' == nullPtr
              then pure (Right ())
              else do
                i <- nodeElement_index n
                x <- peek =<< graphAttributes_x ga n
                y <- peek =<< graphAttributes_y ga n
                w <- peek =<< graphAttributes_width ga n
                h <- peek =<< graphAttributes_height ga n
                let txt =
                      T.pack (show (fromIntegral i :: Int))
                        <> " "
                        <> T.pack (show (realToFrac x :: Double))
                        <> " "
                        <> T.pack (show (realToFrac y :: Double))
                        <> " "
                        <> T.pack (show (realToFrac w :: Double))
                        <> " "
                        <> T.pack (show (realToFrac h :: Double))
                TIO.putStrLn txt
                Left <$> nodeElement_succ n
