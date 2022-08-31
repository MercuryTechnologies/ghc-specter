{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}

module Toolbox.Render.ModuleGraph
  ( -- * Make a visual module graph layout
    layOutGraph,

    -- * Render HTML for the Module Graph tab
    renderModuleGraphTab,

    -- * show textual info:

    -- TODO: they are obsolete and will be moved to a separate module
    formatModuleGraphInfo,
    stat,
    analyze,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( Props,
    classList,
    div,
    height,
    input,
    label,
    onClick,
    onInput,
    onMouseEnter,
    onMouseLeave,
    pre,
    text,
    textProp,
    width,
  )
import Concur.Replica.DOM.Props qualified as DP (checked, name, type_)
import Concur.Replica.SVG qualified as S
import Concur.Replica.SVG.Props qualified as SP
import Control.Monad (void)
import Control.Monad.Extra (loop)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (allocate)
import Data.Bits ((.|.))
import Data.Foldable qualified as F
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
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
import PyF (fmt)
import Replica.VDOM.Types (HTML)
import STD.Deletable (delete)
import Toolbox.Channel
  ( ModuleGraphInfo (..),
    ModuleName,
    SessionInfo (..),
    Timer,
  )
import Toolbox.Server.Types
  ( DetailLevel (..),
    Dimension (..),
    EdgeLayout (..),
    Event (..),
    GraphVisInfo (..),
    ModuleGraphEvent (..),
    ModuleGraphUI (..),
    NodeLayout (..),
    Point (..),
    ServerState (..),
    SubModuleEvent (..),
    UIState (..),
    transposeGraphVis,
  )
import Toolbox.Util.Graph.Builder (makeRevDep)
import Toolbox.Util.Graph.Cluster (filterOutSmallNodes)
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
      modRevDep = makeRevDep modDep
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
      larges = filterOutSmallNodes modDep
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

makePolylineText :: (Point, Point) -> [Point] -> Text
makePolylineText (p0, p1) xys =
  T.intercalate " " (fmap each ([p0] ++ xys ++ [p1]))
  where
    each (Point x y) = [fmt|{x:.2},{y:.2}|]

renderModuleGraphSVG ::
  IntMap ModuleName ->
  Map Text Timer ->
  [(Text, [Text])] ->
  GraphVisInfo ->
  -- | (focused (clicked), hinted (hovered))
  (Maybe Text, Maybe Text) ->
  Widget HTML ModuleGraphEvent
renderModuleGraphSVG nameMap timing clustering grVisInfo (mfocused, mhinted) =
  let Dim canvasWidth canvasHeight = gviCanvasDim grVisInfo
      revNameMap = M.fromList $ fmap swap $ IM.toList nameMap
      nodeLayoutMap =
        IM.fromList $ fmap (\n -> (fst (nodePayload n), n)) $ gviNodes grVisInfo
      -- graph layout parameter
      aFactor = 0.9
      offX = -15
      offYFactor = -1.0
      -- the center of left side of a node
      leftCenter (NodeLayout _ (Point x y) (Dim _ h)) =
        Point (x + offX) (y + h * offYFactor + h + 0.5)
      -- the center of right side of a node
      rightCenter (NodeLayout _ (Point x y) (Dim w h)) =
        Point (x + offX + w * aFactor) (y + h * offYFactor + h + 0.5)
      edge (EdgeLayout _ (src, tgt) (srcPt0, tgtPt0) xys) =
        let (color, swidth) = fromMaybe ("gray", "1") $ do
              hinted <- mhinted
              hintedIdx_ <- M.lookup hinted revNameMap
              hintedIdx <- Just hintedIdx_
              if
                  | src == hintedIdx -> pure ("black", "2")
                  | tgt == hintedIdx -> pure ("black", "2")
                  | otherwise -> Nothing
            -- if source and target nodes cannot be found,
            -- just use coordinates recorded in edge.
            -- TODO: should be handled as error.
            (srcPt, tgtPt) = fromMaybe (srcPt0, tgtPt0) $ do
              srcNode <- IM.lookup src nodeLayoutMap
              tgtNode <- IM.lookup tgt nodeLayoutMap
              -- Left-to-right flow.
              pure (rightCenter srcNode, leftCenter tgtNode)
         in S.polyline
              [ SP.points (makePolylineText (srcPt, tgtPt) xys)
              , SP.stroke color
              , SP.strokeWidth swidth
              , SP.fill "none"
              ]
              []

      box0 (NodeLayout (_, name) (Point x y) (Dim w h)) =
        let color
              | Just name == mfocused = "orange"
              | Just name == mhinted = "honeydew"
              | otherwise = "ivory"
         in S.rect
              [ HoverOnModuleEv (Just name) <$ onMouseEnter
              , HoverOnModuleEv Nothing <$ onMouseLeave
              , ClickOnModuleEv (Just name) <$ onClick
              , SP.x (T.pack $ show (x + offX))
              , SP.y (T.pack $ show (y + h * offYFactor + h - 6))
              , width (T.pack $ show (w * aFactor))
              , height "13"
              , SP.stroke "dimgray"
              , SP.fill color
              ]
              []
      box1 (NodeLayout (_, name) (Point x y) (Dim w h)) =
        S.rect
          [ HoverOnModuleEv (Just name) <$ onMouseEnter
          , HoverOnModuleEv Nothing <$ onMouseLeave
          , ClickOnModuleEv (Just name) <$ onClick
          , SP.x (T.pack $ show (x + offX))
          , SP.y (T.pack $ show (y + h * offYFactor + h + 3))
          , width (T.pack $ show (w * aFactor))
          , height "4"
          , SP.stroke "black"
          , SP.fill "none"
          ]
          []
      box2 (NodeLayout (_, name) (Point x y) (Dim w h)) =
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
              [ HoverOnModuleEv (Just name) <$ onMouseEnter
              , HoverOnModuleEv Nothing <$ onMouseLeave
              , ClickOnModuleEv (Just name) <$ onClick
              , SP.x (T.pack $ show (x + offX))
              , SP.y (T.pack $ show (y + h * offYFactor + h + 3))
              , width (T.pack $ show (w' * aFactor))
              , height "4"
              , SP.fill "blue"
              ]
              []
      moduleText (NodeLayout (_, name) (Point x y) (Dim _w h)) =
        S.text
          [ HoverOnModuleEv (Just name) <$ onMouseEnter
          , HoverOnModuleEv Nothing <$ onMouseLeave
          , ClickOnModuleEv (Just name) <$ onClick
          , SP.x (T.pack $ show (x + offX + 2))
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

renderMainModuleGraph ::
  IntMap ModuleName ->
  Map ModuleName Timer ->
  [(Text, [Text])] ->
  GraphVisInfo ->
  -- | main module graph UI state
  ModuleGraphUI ->
  Widget HTML Event
renderMainModuleGraph nameMap timing clustering grVisInfo mgUI =
  let mclicked = modGraphUIClick mgUI
      mhovered = modGraphUIHover mgUI
   in MainModuleEv
        <$> renderModuleGraphSVG nameMap timing clustering grVisInfo (mclicked, mhovered)

renderSubModuleGraph ::
  IntMap ModuleName ->
  Map ModuleName Timer ->
  [(ModuleName, GraphVisInfo)] ->
  -- | (main module graph UI state, sub module graph UI state)
  (ModuleGraphUI, (DetailLevel, ModuleGraphUI)) ->
  Widget HTML Event
renderSubModuleGraph nameMap timing subgraphs (mainMGUI, (_, subMGUI)) =
  let mainModuleClicked = modGraphUIClick mainMGUI
      subModuleHovered = modGraphUIHover subMGUI
   in case mainModuleClicked of
        Nothing -> text "no module cluster is selected"
        Just selected ->
          case L.lookup selected subgraphs of
            Nothing ->
              text [fmt|cannot find the subgraph for the module cluster {selected}|]
            Just subgraph ->
              let tempclustering = fmap (\(NodeLayout (_, name) _ _) -> (name, [name])) $ gviNodes subgraph
               in SubModuleEv . SubModuleGraphEv
                    <$> renderModuleGraphSVG nameMap timing tempclustering subgraph (mainModuleClicked, subModuleHovered)

renderDetailLevel :: UIState -> Widget HTML Event
renderDetailLevel ui =
  SubModuleEv . SubModuleLevelEv
    <$> div
      [classList [("control", True)]]
      [detail30, detail100, detail300]
  where
    currLevel = fst $ uiSubModuleGraph ui
    mkRadioItem ev txt isChecked =
      label
        [classList [("radio", True)]]
        [ input [DP.type_ "radio", DP.name "detail", DP.checked isChecked, ev <$ onInput]
        , text txt
        ]

    detail30 = mkRadioItem UpTo30 "< 30" (currLevel == UpTo30)
    detail100 = mkRadioItem UpTo100 "< 100" (currLevel == UpTo100)
    detail300 = mkRadioItem UpTo300 "< 300" (currLevel == UpTo300)

renderModuleGraphTab :: UIState -> ServerState -> Widget HTML Event
renderModuleGraphTab ui ss =
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
            ( case serverModuleGraph ss of
                Nothing -> []
                Just grVisInfo ->
                  [ renderMainModuleGraph
                      nameMap
                      timing
                      clustering
                      grVisInfo
                      (uiMainModuleGraph ui)
                  , renderDetailLevel ui
                  , renderSubModuleGraph
                      nameMap
                      timing
                      (serverModuleSubgraph ss)
                      (uiMainModuleGraph ui, uiSubModuleGraph ui)
                  ]
            )

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

  nodeLayout0 <- getAllNodeLayout g ga
  let nodeLayout = mapMaybe replace nodeLayout0
        where
          replace (NodeLayout j pt dim) = do
            i <- IM.lookup j moduleNodeRevIndex
            name <- IM.lookup i nameMap
            pure $ NodeLayout (i, name) pt dim

  edgeLayout0 <- getAllEdgeLayout g ga
  let edgeLayout = mapMaybe replace edgeLayout0
        where
          replace (EdgeLayout k (start, end) srcTgtPts vertices) = do
            startIdx <- IM.lookup start moduleNodeRevIndex
            endIdx <- IM.lookup end moduleNodeRevIndex
            pure (EdgeLayout k (startIdx, endIdx) srcTgtPts vertices)

  let gvisInfo0 = GraphVisInfo canvasDim nodeLayout edgeLayout
  pure $ transposeGraphVis gvisInfo0
