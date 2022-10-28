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
import GHCSpecter.Render.Components.TimingView qualified as TimingView
import GHCSpecter.Render.Util (divClass, spanClass, xmlns)
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
    span,
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
import Prelude hiding (div, span)

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
                [TimingView.renderBlockerLine hoveredMod ttable]
            ]
   in div
        [ style
            [ ("width", "100%")
            , ("height", ss ^. serverSessionInfo . to sessionIsPaused . to widgetHeight)
            , ("position", "relative")
            ]
        ]
        ( [ TimingView.render (ss ^. serverDriverModuleMap) (model ^. modelTiming) ttable
          , div
              [style [("position", "absolute"), ("top", "0"), ("right", "0")]]
              [renderCheckbox (model ^. modelTiming)]
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
