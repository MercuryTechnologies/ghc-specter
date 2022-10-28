-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Render.Timing
  ( render,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( classList,
    height,
    onChange,
    onClick,
    style,
    width,
  )
import Concur.Replica.DOM.Props qualified as DP (checked, name, type_)
import Concur.Replica.SVG.Props qualified as SP
import Control.Lens (to, (^.), _1)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isNothing, maybeToList)
import Data.Text qualified as T
import Data.Time.Clock
  ( secondsToNominalDiffTime,
  )
import GHCSpecter.Channel.Common.Types (ModuleName)
import GHCSpecter.Channel.Outbound.Types
  ( ModuleGraphInfo (..),
    SessionInfo (..),
  )
import GHCSpecter.Data.Map (backwardLookup)
import GHCSpecter.Data.Timing.Types
  ( HasPipelineInfo (..),
    HasTimingTable (..),
    TimingTable,
  )
import GHCSpecter.Render.Components.GraphView qualified as GraphView
import GHCSpecter.Render.Components.TimingView
  ( module2Y,
    renderTimingChart,
    viewPortY,
  )
import GHCSpecter.Render.Util (divClass, xmlns)
import GHCSpecter.Server.Types
  ( HasServerState (..),
    HasTimingState (..),
    ServerState (..),
  )
import GHCSpecter.UI.ConcurReplica.DOM
  ( button,
    div,
    hr,
    input,
    label,
    p,
    text,
  )
import GHCSpecter.UI.ConcurReplica.DOM.Events
  ( onMouseDown,
    onMouseMove,
    onMouseUp,
  )
import GHCSpecter.UI.ConcurReplica.SVG qualified as S
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Constants
  ( timingBarHeight,
    timingHeight,
    timingWidth,
    widgetHeight,
  )
import GHCSpecter.UI.Types
  ( HasTimingUI (..),
    HasUIModel (..),
    TimingUI,
    UIModel,
  )
import GHCSpecter.UI.Types.Event
  ( ComponentTag (TimingBar),
    Event (..),
    MouseEvent (..),
    TimingEvent (..),
  )
import Prelude hiding (div)

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

renderTimingBar ::
  TimingUI ->
  TimingTable ->
  Widget IHTML Event
renderTimingBar tui ttable =
  div [] [svgElement]
  where
    timingInfos = ttable ^. ttableTimingInfos
    nMods = length timingInfos

    topOfBox :: Int -> Int
    topOfBox = round . module2Y . fromIntegral

    (i, _) `isInRange` (y0, y1) =
      let y = topOfBox i
       in y0 <= y && y <= y1

    allItems = zip [0 ..] timingInfos
    filteredItems =
      filter (`isInRange` (viewPortY tui, viewPortY tui + timingHeight)) allItems

    (minI, maxI) =
      let idxs = fmap (^. _1) filteredItems
       in (minimum idxs, maximum idxs)

    convert i = floor @Double (fromIntegral i / fromIntegral nMods * fromIntegral timingWidth)
    handleX :: Int
    handleX = if null filteredItems then 0 else convert minI
    handleWidth :: Int
    handleWidth = if null filteredItems then 0 else convert (maxI - minI + 1)

    background =
      S.rect
        [ MouseEv TimingBar . MouseMove <$> onMouseMove
        , MouseEv TimingBar . MouseDown <$> onMouseDown
        , MouseEv TimingBar . MouseUp <$> onMouseUp
        , SP.x "0"
        , SP.y "0"
        , SP.width (T.pack (show timingWidth))
        , SP.height (T.pack (show timingBarHeight))
        , SP.fill "lightgray"
        ]
        []

    handle =
      S.rect
        [ SP.x (T.pack (show handleX))
        , SP.y "0"
        , SP.width (T.pack (show handleWidth))
        , SP.height (T.pack (show timingBarHeight))
        , SP.stroke "black"
        , SP.fill "white"
        ]
        []
    svgProps =
      [ width (T.pack (show timingWidth))
      , height (T.pack (show timingBarHeight))
      , SP.version "1.1"
      , xmlns
      ]

    svgElement =
      S.svg
        svgProps
        [ S.style [] [text ".small { font: 5px sans-serif; } text { user-select: none; }"]
        , background
        , handle
        ]

renderBlockerLine :: ModuleName -> TimingTable -> Widget IHTML Event
renderBlockerLine hoveredMod ttable =
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
        ( divClass "blocker title" [] [text "blocked by"] :
          fmap (\modu -> p [] [text modu]) upMods
        )
    downstreams =
      div
        []
        ( divClass "blocker title" [] [text "blocking"] :
          fmap (\modu -> p [] [text modu]) downMods
        )

-- | regular timing view mode
renderTimingMode :: UIModel -> ServerState -> Widget IHTML Event
renderTimingMode model ss =
  let ttable =
        fromMaybe
          (ss ^. serverTiming . tsTimingTable)
          (model ^. modelTiming . timingFrozenTable)
      mhoveredMod = model ^. modelTiming . timingUIHoveredModule
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
                [renderBlockerLine hoveredMod ttable]
            ]
   in div
        [ style
            [ ("width", "100%")
            , ("height", ss ^. serverSessionInfo . to sessionIsPaused . to widgetHeight)
            , ("position", "relative")
            ]
        ]
        ( [ renderTimingChart (ss ^. serverDriverModuleMap) (model ^. modelTiming) ttable
          , div
              [style [("position", "absolute"), ("top", "0"), ("right", "0")]]
              [renderCheckbox (model ^. modelTiming)]
          , renderTimingBar (model ^. modelTiming) ttable
          ]
            ++ hoverInfo
        )

renderBlockerGraph :: ServerState -> Widget IHTML Event
renderBlockerGraph ss =
  divClass
    "box"
    [ width (T.pack (show timingWidth))
    , height (T.pack (show timingHeight))
    , style [("overflow", "scroll")]
    ]
    contents
  where
    sessionInfo = ss ^. serverSessionInfo
    drvModMap = ss ^. serverDriverModuleMap
    nameMap = mginfoModuleNameMap $ sessionModuleGraph sessionInfo
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
           in [ TimingEv . BlockerModuleGraphEv
                  <$> GraphView.renderModuleGraph
                    nameMap
                    valueFor
                    blockerGraphViz
                    (Nothing, Nothing)
              ]

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
              [ button [TimingEv ShowBlockerGraph <$ onClick] [text "Update"]
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
