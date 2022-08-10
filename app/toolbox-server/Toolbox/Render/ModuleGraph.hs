{-# LANGUAGE BangPatterns #-}

module Toolbox.Render.ModuleGraph
  ( drawGraph,
    filterOutSmallNodes,
    ogdfTest,
    renderModuleGraph,
  )
where

import Concur.Core (Widget)
import Concur.Replica (div, pre, text)
import Control.Exception (bracket)
import Control.Monad (void, when)
import Control.Monad.Extra (loop, loopM)
import Data.Bits ((.|.))
import Data.Discrimination (inner)
import Data.Discrimination.Grouping (grouping)
import Data.Foldable (for_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Traversable (forM)
import Foreign.C.String (withCString)
import Foreign.Ptr (nullPtr)
import OGDF.Graph
  ( Graph,
    graph_firstNode,
    graph_newEdge,
    newGraph,
  )
import OGDF.GraphAttributes
  ( GraphAttributes,
    newGraphAttributes,
  )
import OGDF.GraphIO (graphIO_drawSVG)
import OGDF.LayoutModule (ILayoutModule (call))
import OGDF.MedianHeuristic (newMedianHeuristic)
import OGDF.NodeElement (NodeElement (..), nodeElement_index, nodeElement_succ)
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
import STD.CppString (newCppString)
import STD.Deletable (delete)
import Toolbox.Channel
  ( ModuleGraphInfo (..),
    SessionInfo (..),
  )
import Toolbox.Server.Types (ServerState (..))
import Toolbox.Util.Graph (reduceGraph)
import Toolbox.Util.OGDF
  ( appendText,
    edgeGraphics,
    getHeight,
    getWidth,
    getX,
    getY,
    newGraphNodeWithSize,
    nodeGraphics,
    nodeLabel,
    nodeStyle,
    setFillColor,
  )
import Prelude hiding (div)

mkRevDep :: [(Int, [Int])] -> IntMap [Int]
mkRevDep deps = L.foldl' step emptyMap deps
  where
    emptyMap = L.foldl' (\(!acc) (i, _) -> IM.insert i [] acc) IM.empty deps
    step !acc (i, js) =
      L.foldl' (\(!acc') j -> IM.insertWith (<>) j [i] acc') acc js

nodeSizeLimit :: Int
nodeSizeLimit = 150

filterOutSmallNodes :: ModuleGraphInfo -> [Int]
filterOutSmallNodes graphInfo =
  let modDep = mginfoModuleDep graphInfo
      modRevDepMap = mkRevDep modDep
      modRevDep = IM.toList modRevDepMap
      modBiDep = concat $ inner grouping joiner fst fst modDep modRevDep
        where
          joiner (i, js) (_, ks) = (i, (js, ks))
   in fmap fst $ filter (\(_, (js, ks)) -> length js + length ks > nodeSizeLimit) modBiDep

analyze :: ModuleGraphInfo -> Text
analyze graphInfo =
  let modDep = mginfoModuleDep graphInfo
      modRevDepMap = mkRevDep modDep
      modRevDep = IM.toList modRevDepMap
      initials = fmap fst $ filter (\(_, js) -> null js) modDep
      terminals = fmap fst $ filter (\(_, js) -> null js) modRevDep
      orphans = initials `L.intersect` terminals
      singles = mapMaybe (\(i, js) -> case js of j : [] -> Just (i, j); _ -> Nothing) modDep
      leg i = loop go ([i], i)
        where
          go (acc', i') =
            case L.lookup i' singles of
              Nothing -> Right acc'
              Just j' -> Left (acc' ++ [j'], j')
      legs = fmap leg (initials L.\\ orphans)
      larges = filterOutSmallNodes graphInfo
      largeNames = mapMaybe (\i -> L.lookup i (mginfoModuleNameMap graphInfo)) larges
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
  let nVtx = length $ mginfoModuleNameMap mgi
      nEdg = sum $ fmap (length . snd) $ mginfoModuleDep mgi
   in (nVtx, nEdg)

formatModuleGraphInfo :: ModuleGraphInfo -> Text
formatModuleGraphInfo mgi =
  let txt1 =
        T.intercalate "\n" . fmap (T.pack . show) $ mginfoModuleNameMap mgi
      txt2 =
        T.intercalate "\n" . fmap (T.pack . show) $ mginfoModuleDep mgi
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

ogdfTest :: Bool -> FilePath -> [Int] -> ModuleGraphInfo -> IO ()
ogdfTest showUnclustered file seeds graphInfo = do
  print graphInfo
  let modNameMap = mginfoModuleNameMap graphInfo
      reducedGraph = reduceGraph (not showUnclustered) seeds graphInfo
  drawGraph file modNameMap reducedGraph

drawGraph :: FilePath -> [(Int, Text)] -> [(Int, [Int])] -> IO ()
drawGraph file nameMap graph = do
  bracket newGraph delete $ \g ->
    bracket (newGA g) delete $ \ga -> do
      moduleNodeMap <-
        IM.fromList . concat
          <$> ( forM graph $ \(i, _) -> do
                  case L.lookup i nameMap of
                    Nothing -> pure []
                    Just name -> do
                      let width = 8 * T.length name
                      node <- newGraphNodeWithSize (g, ga) (width, 15)
                      setFillColor ga node "White"
                      appendText ga node name
                      pure [(i, node)]
              )

      for_ graph $ \(i, js) ->
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

        -- temporary
        withCString file $ \cstr -> do
          str <- newCppString cstr
          _ <- graphIO_drawSVG ga str
          delete str

        n0@(NodeElement n0') <- graph_firstNode g
        when (n0' /= nullPtr) $
          void $
            flip loopM n0 $ \n@(NodeElement n'') ->
              if n'' == nullPtr
                then pure (Right ())
                else do
                  i <- nodeElement_index n
                  x <- getX ga n
                  y <- getY ga n
                  w <- getWidth ga n
                  h <- getHeight ga n
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
