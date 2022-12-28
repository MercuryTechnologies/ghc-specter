module GHCSpecter.Render.Timing (
  render,
) where

import Concur.Core (Widget)
import Concur.Replica (
  classList,
  height,
  onChange,
  onClick,
  style,
  width,
 )
import Concur.Replica.DOM.Props qualified as DP (checked, name, type_)
import Control.Lens (to, (^.), _1)
import Data.List qualified as L
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as T
import Data.Time.Clock (
  secondsToNominalDiffTime,
 )
import GHCSpecter.Channel.Outbound.Types (
  ModuleGraphInfo (..),
  SessionInfo (..),
 )
import GHCSpecter.Data.Map (backwardLookup)
import GHCSpecter.Data.Timing.Types (
  HasPipelineInfo (..),
  HasTimingTable (..),
 )
import GHCSpecter.Render.Components.GraphView qualified as GraphView
import GHCSpecter.Render.Components.TimingView qualified as TimingView
import GHCSpecter.Render.Util (divClass)
import GHCSpecter.Server.Types (
  HasServerState (..),
  HasTimingState (..),
  ServerState (..),
  TimingState (..),
 )
import GHCSpecter.UI.ConcurReplica.DOM (
  button,
  div,
  input,
  label,
  text,
 )
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Constants (
  timingHeight,
  timingWidth,
  widgetHeight,
 )
import GHCSpecter.UI.Types (
  HasTimingUI (..),
  HasUIModel (..),
  TimingUI,
  UIModel,
 )
import GHCSpecter.UI.Types.Event (
  BlockerDetailLevel (..),
  Event (..),
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
   in div
        [ style
            [ ("width", "100%")
            , ("height", ss ^. serverSessionInfo . to sessionIsPaused . to widgetHeight)
            , ("position", "relative")
            ]
        ]
        ( [ TimingView.render (ss ^. serverDriverModuleMap) (model ^. modelTiming) ttable
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


-- | show blocker detail level radio button
-- blocker detail level, # of blocked modules >=2, >=3, >=4, >=5
renderBlockerDetailLevel :: TimingState -> Widget IHTML Event
renderBlockerDetailLevel timing =
  TimingEv . BlockerModuleGraphEv
    <$> div
      [ classList [("control", True)]
      -- , style [("position", "absolute"), ("top", "0"), ("right", "0")]
      ]
      details
  where
    currLevel = timing ^. tsBlockerDetailLevel
    mkRadioItem (txt, lvl) = -- isChecked =
      label
        [classList [("radio", True)]]
        [ input [DP.type_ "radio", DP.name "detail", DP.checked (lvl == currLevel)] -- DP.checked isChecked, ev <$ onInput]
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
