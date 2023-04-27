{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Web.Timing (
  render,
) where

import Concur.Core (Widget)
import Concur.Replica (
  classList,
  height,
  onChange,
  onClick,
  onInput,
  onMouseEnter,
  onMouseLeave,
  style,
  width,
 )
import Concur.Replica.DOM.Props qualified as DP (checked, name, type_)
import Concur.Replica.SVG.Props qualified as SP
import Control.Lens (to, (^.), _1, _2)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isNothing, maybeToList)
import Data.Text qualified as T
import Data.Time.Clock (
  secondsToNominalDiffTime,
 )
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
import GHCSpecter.Channel.Outbound.Types (
  ModuleGraphInfo (..),
  SessionInfo (..),
 )
import GHCSpecter.ConcurReplica.DOM (
  button,
  div,
  hr,
  input,
  label,
  p,
  text,
 )
import GHCSpecter.ConcurReplica.DOM.Events (
  onMouseDown,
  onMouseMove,
  onMouseUp,
 )
import GHCSpecter.ConcurReplica.SVG qualified as S
import GHCSpecter.ConcurReplica.Types (IHTML)
import GHCSpecter.Data.Map (
  BiKeyMap,
  backwardLookup,
 )
import GHCSpecter.Data.Timing.Types (
  HasPipelineInfo (..),
  HasTimingTable (..),
  TimingTable,
 )
import GHCSpecter.Graphics.DSL (
  HitEvent (..),
  Scene (..),
  ViewPort (..),
 )
import GHCSpecter.Server.Types (
  HasModuleGraphState (..),
  HasServerState (..),
  HasTimingState (..),
  ServerState (..),
  TimingState (..),
 )
import GHCSpecter.UI.Components.TimingView qualified as TimingView
import GHCSpecter.UI.Constants (
  timingHeight,
  timingRangeHeight,
  timingWidth,
  widgetHeight,
 )
import GHCSpecter.UI.Types (
  HasTimingUI (..),
  HasUIModel (..),
  HasViewPortInfo (..),
  TimingUI,
  UIModel,
 )
import GHCSpecter.UI.Types.Event (
  BackgroundEvent (RefreshUI),
  BlockerDetailLevel (..),
  BlockerModuleGraphEvent (..),
  Event (..),
  MouseEvent (..),
  TimingEvent (..),
 )
import GHCSpecter.Web.ConcurReplicaSVG (renderPrimitive)
import GHCSpecter.Web.ModuleGraph qualified as ModuleGraph
import GHCSpecter.Web.Util (divClass, xmlns)
import Prelude hiding (div, span)

renderTimingChart ::
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  Widget IHTML Event
renderTimingChart drvModMap tui ttable =
  S.svg
    svgProps
    [ S.style [] [text ".small { font: 5px sans-serif; } text { user-select: none; }"]
    , S.g [] (fmap (renderPrimitive handler) rexp)
    ]
  where
    handler hitEvent =
      catMaybes
        [ fmap (\ev -> TimingEv ev <$ onMouseEnter) (hitEventHoverOn hitEvent)
        , fmap (\ev -> TimingEv ev <$ onMouseLeave) (hitEventHoverOn hitEvent)
        ]
    scene = TimingView.buildTimingChart drvModMap tui ttable
    rexp = sceneElements scene
    vpi = tui ^. timingUIViewPort
    vp = fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)
    svgProps =
      let viewboxProp =
            SP.viewBox . T.intercalate " " . fmap (T.pack . show . floor @Double @Int) $
              [ topLeft vp ^. _1
              , topLeft vp ^. _2
              , timingWidth
              , timingHeight
              ]
          prop1 =
            [ MouseEv . MouseDown <$> onMouseDown
            , MouseEv . MouseUp <$> onMouseUp
            , width (T.pack (show (timingWidth :: Int)))
            , height (T.pack (show (timingHeight :: Int)))
            , viewboxProp
            , SP.version "1.1"
            , xmlns
            ]
          mouseMove
            | tui ^. timingUIHandleMouseMove =
                [(\case Nothing -> BkgEv RefreshUI; Just xy -> MouseEv (MouseMove xy)) <$> onMouseMove]
            | otherwise = []
       in mouseMove ++ prop1

renderMemChart ::
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  Widget IHTML Event
renderMemChart drvModMap tui ttable =
  S.svg
    svgProps
    [ S.style [] [text ".small { font: 5px sans-serif; } text { user-select: none; }"]
    , S.g [] (fmap (renderPrimitive (const [])) rexp)
    ]
  where
    scene = TimingView.buildMemChart drvModMap tui ttable
    rexp = sceneElements scene
    vpi = tui ^. timingUIViewPort
    vp = fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)
    viewboxProp =
      SP.viewBox . T.intercalate " " . fmap (T.pack . show . floor @_ @Int) $
        [ 0
        , topLeft vp ^. _2
        , 300
        , timingHeight
        ]
    svgProps =
      [ width "300"
      , height (T.pack (show (timingHeight :: Int)))
      , viewboxProp
      , SP.version "1.1"
      , xmlns
      ]

renderTimingRange ::
  TimingUI ->
  TimingTable ->
  Widget IHTML Event
renderTimingRange tui ttable =
  div [] [svgElement]
  where
    scene = TimingView.buildTimingRange tui ttable
    rexp = sceneElements scene
    svgProps =
      [ width (T.pack (show (timingWidth :: Int)))
      , height (T.pack (show (timingRangeHeight :: Int)))
      , SP.version "1.1"
      , xmlns
      ]
    svgElement =
      S.svg
        svgProps
        [ S.style [] [text ".small { font: 5px sans-serif; } text { user-select: none; }"]
        , S.g [] (fmap (renderPrimitive (const [])) rexp)
        ]

renderBlockers :: ModuleName -> TimingTable -> Widget IHTML Event
renderBlockers hoveredMod ttable =
  divClass "blocker" [] [selected, upstream, hr [], downstreams]
  where
    upMods =
      maybeToList (M.lookup hoveredMod (ttable ^. ttableBlockingUpstreamDependency))
    downMods =
      fromMaybe [] (M.lookup hoveredMod (ttable ^. ttableBlockedDownstreamDependency))

    selected =
      divClass "box" [] [p [] [text hoveredMod]]
    upstream =
      div
        []
        ( divClass "blocker title" [] [text "blocked by"]
            : fmap (\modu -> p [] [text modu]) upMods
        )
    downstreams =
      div
        []
        ( divClass "blocker title" [] [text "blocking"]
            : fmap (\modu -> p [] [text modu]) downMods
        )

renderTimingView ::
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  Widget IHTML Event
renderTimingView drvModMap tui ttable =
  divClass
    "box"
    []
    ( [ divClass
          "columns"
          []
          [ divClass
              "box column is-four-fifths"
              [style [("overflow", "hidden")]]
              [ renderTimingChart drvModMap tui ttable
              ]
          , divClass
              "box column is-one-fifth"
              [style [("overflow", "hidden")]]
              [renderMemChart drvModMap tui ttable]
          ]
      , renderTimingRange tui ttable
      ]
        ++ hoverInfo
    )
  where
    mhoveredMod = tui ^. timingUIHoveredModule
    hoverInfo =
      case mhoveredMod of
        Nothing -> []
        Just hoveredMod ->
          [ divClass
              "box"
              [ style
                  [ ("width", "150px")
                  , ("height", "120px")
                  , ("position", "absolute")
                  , ("bottom", "0")
                  , ("left", "0")
                  , ("background", "ivory")
                  , ("overflow", "hidden")
                  ]
              ]
              [renderBlockers hoveredMod ttable]
          ]

buttonShowBlocker :: TimingUI -> Widget IHTML Event
buttonShowBlocker tui = divClass "control" [] [button']
  where
    button'
      | tui ^. timingUIBlockerGraph =
          button [TimingEv CloseBlockerGraph <$ onClick] [text "Back to Timing Graph"]
      | otherwise =
          button [TimingEv ShowBlockerGraph <$ onClick] [text "Show Blocker Graph"]

renderCheckbox :: TimingUI -> Widget IHTML Event
renderCheckbox tui =
  div
    []
    [ buttonToCurrent
    , buttonFlow
    , buttonShowBlocker tui
    , checkPartition
    , checkHowParallel
    ]
  where
    isPartitioned = tui ^. timingUIPartition
    howParallel = tui ^. timingUIHowParallel
    mkEvent f b = TimingEv (f (not b)) <$ onChange
    buttonToCurrent =
      divClass
        "control"
        []
        [button [TimingEv ToCurrentTime <$ onClick] [text "To Current Time"]]
    buttonFlow = divClass "control" [] [button']
      where
        button'
          | isNothing (tui ^. timingFrozenTable) =
              button [TimingEv (TimingFlow False) <$ onClick] [text "Freeze"]
          | otherwise =
              button [TimingEv (TimingFlow True) <$ onClick] [text "Thaw"]
    checkPartition =
      div
        [classList [("control", True)]]
        [ label
            [classList [("checkbox", True)]]
            [ input
                [ DP.type_ "checkbox"
                , DP.name "partition"
                , DP.checked isPartitioned
                , mkEvent UpdatePartition isPartitioned
                ]
            , text "Partition"
            ]
        ]
    checkHowParallel =
      div
        [classList [("control", True)]]
        [ label
            [classList [("checkbox", True)]]
            [ input
                [ DP.type_ "checkbox"
                , DP.name "howparallel"
                , DP.checked howParallel
                , mkEvent UpdateParallel howParallel
                ]
            , text "Parallelism"
            ]
        ]

-- | regular timing view mode
renderTimingMode :: UIModel -> ServerState -> Widget IHTML Event
renderTimingMode model ss =
  let ttable =
        fromMaybe
          (ss ^. serverTiming . tsTimingTable)
          (model ^. modelTiming . timingFrozenTable)
   in div
        [ style
            [ ("width", "100%")
            , ("height", ss ^. serverSessionInfo . to sessionIsPaused . to widgetHeight)
            , ("position", "relative")
            ]
        ]
        ( [ renderTimingView (ss ^. serverDriverModuleMap) (model ^. modelTiming) ttable
          , div
              [ style
                  [ ("position", "absolute")
                  , ("top", "0")
                  , ("right", "0")
                  , ("background-color", "white")
                  ]
              ]
              [renderCheckbox (model ^. modelTiming)]
          ]
        )

renderBlockerGraph :: ServerState -> Widget IHTML Event
renderBlockerGraph ss =
  divClass
    "box"
    [ width (T.pack (show (timingWidth :: Int)))
    , height (T.pack (show (timingHeight :: Int)))
    , style [("overflow", "scroll")]
    ]
    contents
  where
    drvModMap = ss ^. serverDriverModuleMap
    nameMap = ss ^. serverModuleGraphState . mgsModuleGraphInfo . to mginfoModuleNameMap
    ttable = ss ^. serverTiming . tsTimingTable
    maxTime =
      case ttable ^. ttableTimingInfos of
        [] -> secondsToNominalDiffTime 1.0
        ts -> maximum (fmap (\(_, t) -> t ^. plEnd . _1 - t ^. plStart . _1) ts)
    mblockerGraphViz = ss ^. serverTiming . tsBlockerGraphViz
    contents =
      case mblockerGraphViz of
        Nothing -> []
        Just blockerGraphViz ->
          let valueFor name =
                fromMaybe 0 $ do
                  i <- backwardLookup name drvModMap
                  t <- L.lookup i (ttable ^. ttableTimingInfos)
                  pure $ realToFrac ((t ^. plEnd . _1 - t ^. plStart . _1) / maxTime)
           in [ TimingEv . BlockerModuleGraphEv . BMGGraph
                  <$> ModuleGraph.renderModuleGraph
                    nameMap
                    valueFor
                    blockerGraphViz
                    (Nothing, Nothing)
              ]

-- | show blocker detail level radio button
-- blocker detail level, # of blocked modules >=2, >=3, >=4, >=5
renderBlockerDetailLevel :: TimingState -> Widget IHTML Event
renderBlockerDetailLevel timing =
  TimingEv . BlockerModuleGraphEv <$> div [classList [("control", True)]] details
  where
    currLevel = timing ^. tsBlockerDetailLevel
    mkRadioItem (txt, lvl) =
      label
        [classList [("radio", True)]]
        [ input
            [ DP.type_ "radio"
            , DP.name "detail"
            , DP.checked (lvl == currLevel)
            , BMGUpdateLevel lvl <$ onInput
            ]
        , text txt
        ]
    details = fmap mkRadioItem [(">=2", Blocking2), (">=3", Blocking3), (">=4", Blocking4), (">=5", Blocking5)]

-- | blocker graph mode
renderBlockerGraphMode :: UIModel -> ServerState -> Widget IHTML Event
renderBlockerGraphMode model ss =
  div
    [ style
        [ ("width", "100%")
        , ("height", ss ^. serverSessionInfo . to sessionIsPaused . to widgetHeight)
        , ("position", "relative")
        , ("overflow", "auto")
        ]
    ]
    ( [ renderBlockerGraph ss
      , div
          [style [("position", "absolute"), ("top", "0"), ("right", "0")]]
          [ div
              []
              [ renderBlockerDetailLevel (ss ^. serverTiming)
              , button [TimingEv ShowBlockerGraph <$ onClick] [text "Update"]
              , buttonShowBlocker (model ^. modelTiming)
              ]
          ]
      ]
    )

render :: UIModel -> ServerState -> Widget IHTML Event
render model ss
  | model ^. modelTiming . timingUIBlockerGraph =
      renderBlockerGraphMode model ss
  | otherwise =
      renderTimingMode model ss
