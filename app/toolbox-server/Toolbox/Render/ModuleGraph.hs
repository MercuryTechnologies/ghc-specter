{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}

module Toolbox.Render.ModuleGraph
  ( layOutGraph,
    renderModuleGraph,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( Props,
    classList,
    div,
    height,
    onClick,
    onMouseEnter,
    onMouseLeave,
    pre,
    text,
    textProp,
    width,
  )
import qualified Concur.Replica.SVG as S
import qualified Concur.Replica.SVG.Props as SP
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Extra (loop)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (allocate)
import Data.Bits ((.|.))
import qualified Data.Foldable as F
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)
import Foreign.C.String (withCString)
import OGDF.Graph
  ( Graph,
    graph_newEdge,
    newGraph,
  )
import OGDF.GraphAttributes
  ( GraphAttributes,
    newGraphAttributes,
  )
import OGDF.GraphIO (graphIO_write)
import OGDF.NodeElement (nodeElement_index)
import PyF (fmt)
import Replica.VDOM.Types (HTML)
import STD.Deletable (delete)
import STD.CppString (newCppString)
import Toolbox.Channel
  ( ModuleGraphInfo (..),
    ModuleName,
    SessionInfo (..),
    Timer,
  )
import Toolbox.Server.Types
  ( Dimension (..),
    EdgeLayout (..),
    Event (..),
    GraphVisInfo (..),
    NodeLayout (..),
    Point (..),
    ServerState (..),
    UIState (..),
    transposeGraphVis,
  )
import Toolbox.Util.Graph
  ( filterOutSmallNodes,
    mkRevDep,
  )
import Toolbox.Util.OGDF
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
import Prelude hiding (div)

xmlns :: Props a
xmlns = textProp "xmlns" "http://www.w3.org/2000/svg"

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

makePolylineText :: [Point] -> Text
makePolylineText xys = T.intercalate " " (fmap each xys)
  where
    each (Point x y) = [fmt|{x:.2},{y:.2}|]

renderModuleGraphSVG ::
  IntMap ModuleName ->
  Map Text Timer ->
  [(Text, [Text])] ->
  GraphVisInfo ->
  Maybe Text ->
  Widget HTML Event
renderModuleGraphSVG nameMap timing clustering grVisInfo mhovered =
  let Dim canvasWidth canvasHeight = gviCanvasDim grVisInfo
      revNameMap = M.fromList $ fmap swap $ IM.toList nameMap
      edge (EdgeLayout _ (start, end) xys) =
        let (color, swidth) = fromMaybe ("gray", "1") $ do
              hovered <- mhovered
              hoveredIdx_ <- M.lookup hovered revNameMap
              hoveredIdx <- Just hoveredIdx_
              if
                  | start == hoveredIdx -> pure ("black", "2")
                  | end == hoveredIdx -> pure ("black", "2")
                  | otherwise -> Nothing
         in S.polyline
              [ SP.points (makePolylineText xys)
              , SP.stroke color
              , SP.strokeWidth swidth
              , SP.fill "none"
              ]
              []
      aFactor = 0.9
      offXFactor = -0.3
      offYFactor = -0.9
      box0 (NodeLayout name (Point x y) (Dim w h)) =
        S.rect
          [ SP.x (T.pack $ show (x + w * offXFactor))
          , SP.y (T.pack $ show (y + h * offYFactor + h - 6))
          , width (T.pack $ show (w * aFactor))
          , height "13"
          , SP.stroke "dimgray"
          , SP.fill $ if Just name == mhovered then "honeydew" else "ivory"
          ]
          []
      box1 (NodeLayout _ (Point x y) (Dim w h)) =
        S.rect
          [ SP.x (T.pack $ show (x + w * offXFactor))
          , SP.y (T.pack $ show (y + h * offYFactor + h + 3))
          , width (T.pack $ show (w * aFactor))
          , height "4"
          , SP.stroke "black"
          , SP.fill "none"
          ]
          []
      box2 (NodeLayout name (Point x y) (Dim w h)) =
        let ratio = fromMaybe 0 $ do
              cluster <- L.lookup name clustering
              let nTot = length cluster
              if nTot == 0
                then Nothing
                else do
                  let compiled = filter (\j -> j `M.member` timing) cluster
                      nCompiled = length compiled
                  pure (fromIntegral nCompiled / fromIntegral nTot)
            w' = ratio * w
         in S.rect
              [ SP.x (T.pack $ show (x + w * offXFactor))
              , SP.y (T.pack $ show (y + h * offYFactor + h + 3))
              , width (T.pack $ show (w' * aFactor))
              , height "4"
              , SP.fill "blue"
              ]
              []
      moduleText (NodeLayout name (Point x y) (Dim w h)) =
        S.text
          [ HoverOnModuleEv (Just name) <$ onMouseEnter
          , HoverOnModuleEv Nothing <$ onMouseLeave
          , ClickOnModuleEv (Just name) <$ onClick
          , SP.x (T.pack $ show (x + w * offXFactor + 2))
          , SP.y (T.pack $ show (y + h * offYFactor + h))
          , classList [("small", True)]
          ]
          [text name]

      edges = fmap edge $ gviEdges grVisInfo
      nodes =
        concatMap (\x -> [box0 x, box1 x, box2 x, moduleText x]) (gviNodes grVisInfo)

      svgElement =
        S.svg
          [ width (T.pack (show (canvasWidth + 100)))
          , SP.viewBox
              ( "0 0 "
                  <> T.pack (show (canvasWidth + 100)) -- i don't understand why it's incorrect
                  <> " "
                  <> T.pack (show (canvasHeight + 100))
              )
          , SP.version "1.1"
          , xmlns
          ]
          (S.style [] [text ".small { font: 6px sans-serif; }"] : (edges ++ nodes))
   in div [classList [("is-fullwidth", True)]] [svgElement]

renderSubgraph ::
  Map ModuleName Timer ->
  [(ModuleName, GraphVisInfo)] ->
  Maybe Text ->
  Widget HTML Event
renderSubgraph timing subgraphs mselected =
  case mselected of
    Nothing -> text "no module cluster is selected"
    Just selected ->
      case L.lookup selected subgraphs of
        Nothing ->
          text [fmt|cannot find the subgraph for the module cluster {selected}|]
        Just subgraph ->
          let tempclustering = fmap (\(NodeLayout name _ _) -> (name, [name])) $ gviNodes subgraph
           in renderModuleGraphSVG mempty timing tempclustering subgraph Nothing

renderModuleGraph :: UIState -> ServerState -> Widget HTML Event
renderModuleGraph ui ss =
  let sessionInfo = serverSessionInfo ss
      nameMap = mginfoModuleNameMap $ sessionModuleGraph sessionInfo
      timing = serverTiming ss
      clustering = serverModuleClustering ss
   in case sessionStartTime sessionInfo of
        Nothing ->
          pre [] [text "GHC Session has not been started"]
        Just _ ->
          div
            []
            ( ( case serverModuleGraph ss of
                  Nothing -> []
                  Just grVisInfo ->
                    [ renderModuleGraphSVG nameMap timing clustering grVisInfo (uiModuleHover ui)
                    , renderSubgraph timing (serverModuleSubgraph ss) (uiModuleClick ui)
                    ]
              )
                ++ [pre [] [text $ formatModuleGraphInfo (sessionModuleGraph sessionInfo)]]
            )

newGA :: Graph -> IO GraphAttributes
newGA g = newGraphAttributes g (nodeGraphics .|. edgeGraphics .|. nodeLabel .|. nodeStyle)

layOutGraph :: Maybe FilePath -> IntMap ModuleName -> IntMap [Int] -> IO GraphVisInfo
layOutGraph mfile nameMap graph = runGraphLayouter $ do
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
  F.for_ mfile $ \file -> liftIO $ do
    withCString file $ \outputGMLFileNameCstr ->
      bracket (newCppString outputGMLFileNameCstr) delete $ \outputGMLFileName ->
        graphIO_write ga outputGMLFileName
  
  doSugiyamaLayout ga

  canvasDim <- getCanvasDim ga

  nodeLayout0 <- getAllNodeLayout g ga
  let nodeLayout = mapMaybe replace nodeLayout0
        where
          replace (NodeLayout j pt dim) = do
            i <- IM.lookup j moduleNodeRevIndex
            name <- IM.lookup i nameMap
            pure $ NodeLayout name pt dim

  edgeLayout0 <- getAllEdgeLayout g ga
  let edgeLayout = mapMaybe replace edgeLayout0
        where
          replace (EdgeLayout k (start, end) vertices) = do
            startIdx <- IM.lookup start moduleNodeRevIndex
            endIdx <- IM.lookup end moduleNodeRevIndex
            pure (EdgeLayout k (startIdx, endIdx) vertices)

  let gvisInfo0 = GraphVisInfo canvasDim nodeLayout edgeLayout
  pure $ transposeGraphVis gvisInfo0
