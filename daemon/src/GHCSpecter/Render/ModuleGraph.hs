{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}

module GHCSpecter.Render.ModuleGraph
  ( -- * Make a visual module graph layout
    layOutGraph,

    -- * Render HTML for the Module Graph tab
    render,

    -- * show textual info:

    -- TODO: they are obsolete and will be moved to a separate module
    formatModuleGraphInfo,
    stat,
    analyze,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( classList,
    height,
    onClick,
    onInput,
    onMouseEnter,
    onMouseLeave,
    width,
  )
import Concur.Replica.DOM.Props qualified as DP (checked, name, type_)
import Concur.Replica.SVG.Props qualified as SP
import Control.Error.Util (note)
import Control.Lens ((^.), _1)
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
import GHCSpecter.Channel
  ( ModuleGraphInfo (..),
    ModuleName,
    SessionInfo (..),
    Timer,
  )
import GHCSpecter.Render.Util (xmlns)
import GHCSpecter.Server.Types
  ( Dimension (..),
    EdgeLayout (..),
    GraphVisInfo (..),
    HasGraphVisInfo (..),
    HasModuleGraphState (..),
    HasNodeLayout (..),
    HasServerState (..),
    NodeLayout (..),
    Point (..),
    ServerState (..),
    transposeGraphVis,
  )
import GHCSpecter.UI.ConcurReplica.DOM
  ( div,
    input,
    label,
    pre,
    text,
  )
import GHCSpecter.UI.ConcurReplica.DOM.Events (onMouseMove)
import GHCSpecter.UI.ConcurReplica.SVG qualified as S
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Types
  ( HasModuleGraphUI (..),
    HasUIState (..),
    HasUIView (..),
    ModuleGraphUI (..),
    UIState (..),
  )
import GHCSpecter.UI.Types.Event
  ( DetailLevel (..),
    Event (..),
    ModuleGraphEvent (..),
    SubModuleEvent (..),
  )
import GHCSpecter.Util.Graph.Builder (makeRevDep)
import GHCSpecter.Util.Graph.Cluster (filterOutSmallNodes)
import GHCSpecter.Util.OGDF
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
import Text.Printf (printf)
import Prelude hiding (div)

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
    each (Point x y) = T.pack $ printf "%.2f,%.2f" x y

renderModuleGraphSVG ::
  IntMap ModuleName ->
  Map Text Timer ->
  [(Text, [Text])] ->
  GraphVisInfo ->
  -- | (focused (clicked), hinted (hovered))
  (Maybe Text, Maybe Text) ->
  Widget IHTML ModuleGraphEvent
renderModuleGraphSVG
  nameMap
  timing
  clustering
  grVisInfo
  (mfocused, mhinted) =
    let Dim canvasWidth canvasHeight = grVisInfo ^. gviCanvasDim
        revNameMap = M.fromList $ fmap swap $ IM.toList nameMap
        nodeLayoutMap =
          IM.fromList $ fmap (\n -> (n ^. nodePayload . _1, n)) (grVisInfo ^. gviNodes)
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

        edges = fmap edge (grVisInfo ^. gviEdges)
        nodes =
          concatMap (\x -> [box0 x, box1 x, box2 x, moduleText x]) (grVisInfo ^. gviNodes)

        svgProps =
          let svgProps0 =
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
           in (DummyEv <$> onMouseMove) : svgProps0
        svgElement =
          S.svg
            svgProps
            (S.style [] [text ".small { font: 6px sans-serif; }"] : (edges ++ nodes))
     in div [classList [("is-fullwidth", True)]] [svgElement]

renderMainModuleGraph ::
  IntMap ModuleName ->
  Map ModuleName Timer ->
  [(Text, [Text])] ->
  GraphVisInfo ->
  -- | main module graph UI state
  ModuleGraphUI ->
  Widget IHTML Event
renderMainModuleGraph
  nameMap
  timing
  clustering
  grVisInfo
  mgUI =
    let mclicked = mgUI ^. modGraphUIClick
        mhovered = mgUI ^. modGraphUIHover
     in MainModuleEv
          <$> renderModuleGraphSVG
            nameMap
            timing
            clustering
            grVisInfo
            (mclicked, mhovered)

renderSubModuleGraph ::
  IntMap ModuleName ->
  Map ModuleName Timer ->
  [(DetailLevel, [(ModuleName, GraphVisInfo)])] ->
  -- | (main module graph UI state, sub module graph UI state)
  (ModuleGraphUI, (DetailLevel, ModuleGraphUI)) ->
  Widget IHTML Event
renderSubModuleGraph
  nameMap
  timing
  subgraphs
  (mainMGUI, (detailLevel, subMGUI)) =
    let mainModuleClicked = mainMGUI ^. modGraphUIClick
        subModuleHovered = subMGUI ^. modGraphUIHover
        esubgraph = do
          selected <-
            note "no module cluster is selected" mainModuleClicked
          subgraphsAtTheLevel <-
            note (printf "%s subgraph is not computed" (show detailLevel)) (L.lookup detailLevel subgraphs)
          subgraph <-
            note
              (printf "cannot find the subgraph for the module cluster %s" (T.unpack selected))
              (L.lookup selected subgraphsAtTheLevel)
          pure subgraph
     in case esubgraph of
          Left err -> text (T.pack err)
          Right subgraph ->
            let tempclustering =
                  fmap
                    (\(NodeLayout (_, name) _ _) -> (name, [name]))
                    (subgraph ^. gviNodes)
             in SubModuleEv . SubModuleGraphEv
                  <$> renderModuleGraphSVG
                    nameMap
                    timing
                    tempclustering
                    subgraph
                    (mainModuleClicked, subModuleHovered)

renderDetailLevel :: UIState -> Widget IHTML Event
renderDetailLevel ui =
  SubModuleEv . SubModuleLevelEv
    <$> div
      [classList [("control", True)]]
      [detail30, detail100, detail300]
  where
    currLevel = ui ^. uiView . uiSubModuleGraph . _1
    mkRadioItem ev txt isChecked =
      label
        [classList [("radio", True)]]
        [ input [DP.type_ "radio", DP.name "detail", DP.checked isChecked, ev <$ onInput]
        , text txt
        ]

    detail30 = mkRadioItem UpTo30 "< 30" (currLevel == UpTo30)
    detail100 = mkRadioItem UpTo100 "< 100" (currLevel == UpTo100)
    detail300 = mkRadioItem UpTo300 "< 300" (currLevel == UpTo300)

-- | top-level render function for Module Graph tab
render :: UIState -> ServerState -> Widget IHTML Event
render ui ss =
  let sessionInfo = ss ^. serverSessionInfo
      nameMap = mginfoModuleNameMap $ sessionModuleGraph sessionInfo
      timing = ss ^. serverTiming
      mgs = ss ^. serverModuleGraphState
      clustering = mgs ^. mgsClustering
   in case sessionStartTime sessionInfo of
        Nothing ->
          pre [] [text "GHC Session has not been started"]
        Just _ ->
          div
            []
            ( case mgs ^. mgsClusterGraph of
                Nothing -> []
                Just grVisInfo ->
                  [ renderMainModuleGraph
                      nameMap
                      timing
                      clustering
                      grVisInfo
                      (ui ^. uiView . uiMainModuleGraph)
                  , renderDetailLevel ui
                  , renderSubModuleGraph
                      nameMap
                      timing
                      (mgs ^. mgsSubgraph)
                      (ui ^. uiView . uiMainModuleGraph, ui ^. uiView . uiSubModuleGraph)
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
