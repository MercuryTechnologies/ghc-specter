{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.UI.Constants
  ( -- * time interval
    chanUpdateInterval,
    uiUpdateInterval,
    tickInterval,

    -- * Timing view
    timingMaxWidth,
    timingWidth,
    timingHeight,
    timingRangeHeight,

    -- * module graph
    modGraphWidth,
    modGraphHeight,

    -- * session
    sessionModStatusDim,

    -- * console
    consoleInputHeight,

    -- * global
    canvasDim,
    tabHeight,

    -- * web
    widgetHeight,

    -- * widget config types
    WidgetConfig (..),
    emptyWidgetConfig,

    -- * default widget config
    appWidgetConfig,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)
import GHCSpecter.Graphics.DSL (ViewPort (..))

chanUpdateInterval :: NominalDiffTime
chanUpdateInterval = secondsToNominalDiffTime (fromRational (1 / 2))

uiUpdateInterval :: NominalDiffTime
uiUpdateInterval = secondsToNominalDiffTime (fromRational (1 / 10))

tickInterval :: NominalDiffTime
tickInterval = secondsToNominalDiffTime 1

timingMaxWidth :: (Num a) => a
timingMaxWidth = 10240

timingWidth :: (Num a) => a
timingWidth = 1024

timingHeight :: (Num a) => a
timingHeight = 500

timingRangeHeight :: (Num a) => a
timingRangeHeight = 10

modGraphWidth :: (Num a) => a
modGraphWidth = 1024

modGraphHeight :: (Num a) => a
modGraphHeight = 500

sessionModStatusDim :: (Num a) => (a, a)
sessionModStatusDim = (250, 80)

canvasDim :: (Num a) => (a, a)
canvasDim = (1440, 768)

tabHeight :: (Num a) => a
tabHeight = 15

consolePanelHeight :: (Num a) => a
consolePanelHeight = 368

consoleInputHeight :: (Num a) => a
consoleInputHeight = 15

-- TODO: this web-specific code should be located elsewhere.
widgetHeight :: Bool -> Text
widgetHeight isPaused
  | isPaused = "50vh"
  | otherwise = "95vh"

-- | Each widget placing in the global canvas.
data WidgetConfig = WidgetConfig
  { _wcfgTopLevel :: Map Text ViewPort,
    _wcfgSession :: Map Text ViewPort,
    _wcfgModuleGraph :: Map Text ViewPort,
    _wcfgSourceView :: Map Text ViewPort,
    _wcfgTiming :: Map Text ViewPort
  }

emptyWidgetConfig :: WidgetConfig
emptyWidgetConfig =
  WidgetConfig
    { _wcfgTopLevel = Map.empty,
      _wcfgSession = Map.empty,
      _wcfgModuleGraph = Map.empty,
      _wcfgSourceView = Map.empty,
      _wcfgTiming = Map.empty
    }

-- TODO: use type-level literal or something to be more safe.
-- TODO: also replace each repeated values for adjacent elements with a single variable.
-- TODO: THIS WHOLE CONFIG IS NOW OBSOLETE.
appWidgetConfig :: WidgetConfig
appWidgetConfig =
  WidgetConfig
    { _wcfgTopLevel =
        Map.fromList
          [ ("tab", ViewPort (0, 0) (fst canvasDim, tabHeight)),
            ( "console-tab",
              ViewPort (0, snd canvasDim - consolePanelHeight) (fst canvasDim, snd canvasDim - consolePanelHeight + tabHeight)
            ),
            ( "console-main",
              ViewPort (0, 0) (fst canvasDim, consolePanelHeight)
            ),
            ( "console-input",
              ViewPort (0, snd canvasDim - consoleInputHeight) canvasDim
            ),
            ( "console-help",
              ViewPort (fst canvasDim - 200, snd canvasDim - consolePanelHeight + tabHeight) (fst canvasDim, snd canvasDim - consolePanelHeight + tabHeight + 200)
            )
          ],
      _wcfgSession =
        Map.fromList
          [ ( "module-status",
              ViewPort
                (fst canvasDim - fst sessionModStatusDim, tabHeight)
                (fst canvasDim, snd sessionModStatusDim + tabHeight)
            ),
            ( "session-main",
              ViewPort (5, tabHeight + 5) canvasDim
            ),
            ( "session-process",
              ViewPort (5, 100) (fst canvasDim - fst sessionModStatusDim - 5, 400)
            ),
            ( "session-rts",
              ViewPort (5, 405) (fst canvasDim - fst sessionModStatusDim - 5, 700)
            ),
            ( "session-button",
              ViewPort (1000, tabHeight) (1100, tabHeight + 15)
            )
          ],
      _wcfgModuleGraph =
        Map.fromList
          [ ("main-module-graph", ViewPort (0, 0) (modGraphWidth, modGraphHeight)),
            ("sub-module-graph", ViewPort (0, 0) (modGraphWidth, 0.5 * modGraphHeight))
          ],
      _wcfgSourceView =
        Map.fromList
          [ ("module-tree", ViewPort (0, tabHeight) (0.2 * fst canvasDim, snd canvasDim)),
            ("source-view", ViewPort (0, 0) (0.5 * fst canvasDim, snd canvasDim)),
            ("supple-view", ViewPort (0, 0) (0.5 * fst canvasDim, snd canvasDim)),
            ("supple-view-contents", ViewPort (0, 0) (0.5 * fst canvasDim, snd canvasDim))
          ],
      _wcfgTiming =
        Map.fromList
          [ ("timing-chart", ViewPort (0, 0) (0.85 * timingWidth, timingHeight)),
            ("mem-chart", ViewPort (0.85 * timingWidth, 0) (timingWidth, timingHeight)),
            ("timing-range", ViewPort (0, timingHeight) (timingWidth, timingHeight + timingRangeHeight)),
            ("blocker-module-graph", ViewPort (0, 0) (timingWidth, timingHeight))
          ]
    }
