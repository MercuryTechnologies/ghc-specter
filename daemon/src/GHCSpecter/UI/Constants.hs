{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
    HasWidgetConfig (..),
    emptyWidgetConfig,

    -- * default widget config
    appWidgetConfig,
  )
where

import Control.Lens (makeClassy, to, (^.), _1, _2)
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
timingHeight = 600

timingRangeHeight :: (Num a) => a
timingRangeHeight = 10

modGraphWidth :: (Num a) => a
modGraphWidth = 1440

modGraphHeight :: (Num a) => a
modGraphHeight = 768

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

makeClassy ''WidgetConfig

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
appWidgetConfig :: WidgetConfig
appWidgetConfig =
  WidgetConfig
    { _wcfgTopLevel =
        Map.fromList
          [ ("tab", ViewPort (0, 0) (canvasDim ^. _1, tabHeight)),
            ( "console-tab",
              ViewPort (0, canvasDim ^. _2 - consolePanelHeight) (canvasDim ^. _1, canvasDim ^. _2 - consolePanelHeight + tabHeight)
            ),
            ( "console-main",
              ViewPort (0, canvasDim ^. _2 - consolePanelHeight + tabHeight) (canvasDim ^. _1, canvasDim ^. _2 - consoleInputHeight)
            ),
            ( "console-input",
              ViewPort (0, canvasDim ^. _2 - consoleInputHeight) canvasDim
            ),
            ( "console-help",
              ViewPort (canvasDim ^. _1 - 200, canvasDim ^. _2 - consolePanelHeight + tabHeight) (canvasDim ^. _1, canvasDim ^. _2 - consolePanelHeight + tabHeight + 200)
            )
          ],
      _wcfgSession =
        Map.fromList
          [ ( "module-status",
              ViewPort
                (canvasDim ^. _1 - sessionModStatusDim ^. _1, tabHeight)
                (canvasDim ^. _1, sessionModStatusDim ^. _2 + tabHeight)
            ),
            ( "session-main",
              ViewPort (5, tabHeight + 5) canvasDim
            ),
            ( "session-process",
              ViewPort (5, 100) (canvasDim ^. _1 - sessionModStatusDim ^. _1 - 5, 400)
            ),
            ( "session-rts",
              ViewPort (5, 405) (canvasDim ^. _1 - sessionModStatusDim ^. _1 - 5, 700)
            ),
            ( "session-button",
              ViewPort (1000, tabHeight) (1100, tabHeight + 15)
            )
          ],
      _wcfgModuleGraph =
        Map.fromList
          [ ("main-module-graph", ViewPort (0, tabHeight) (modGraphWidth, 0.5 * modGraphHeight + tabHeight)),
            ("sub-module-graph", ViewPort (0, 0.5 * modGraphHeight + tabHeight) (canvasDim ^. _1, canvasDim ^. _2))
          ],
      _wcfgSourceView =
        Map.fromList
          [ ("module-tree", ViewPort (0, tabHeight) (canvasDim ^. _1 . to (* 0.2), canvasDim ^. _2)),
            ("source-view", ViewPort (canvasDim ^. _1 . to (* 0.2), tabHeight) (canvasDim ^. _1 . to (* 0.6), canvasDim ^. _2)),
            ("supple-view", ViewPort (canvasDim ^. _1 . to (* 0.6), tabHeight) (canvasDim ^. _1, canvasDim ^. _2)),
            ("supple-view-tab", ViewPort (canvasDim ^. _1 . to (* 0.6), tabHeight) (canvasDim ^. _1, tabHeight + tabHeight)),
            ("supple-view-contents", ViewPort (canvasDim ^. _1 . to (* 0.6), tabHeight + tabHeight) (canvasDim ^. _1, canvasDim ^. _2))
          ],
      _wcfgTiming =
        Map.fromList
          [ ("timing-chart", ViewPort (0, 0) (0.85 * timingWidth, timingHeight)),
            ("mem-chart", ViewPort (0.85 * timingWidth, 0) (timingWidth, timingHeight)),
            ("timing-range", ViewPort (0, timingHeight) (timingWidth, timingHeight + timingRangeHeight))
            -- ("blockers", ViewPort (0, timingHeight + timingRangeHeight + tabHeight) (300, canvasDim ^. _2))
          ]
          {-
          [ ("timing-chart", ViewPort (0, tabHeight) (0.8 * timingWidth, timingHeight + tabHeight)),
            ("mem-chart", ViewPort (0.8 * timingWidth, tabHeight) (timingWidth, timingHeight + tabHeight)),
            ("timing-range", ViewPort (0, timingHeight + tabHeight) (timingWidth, timingHeight + timingRangeHeight + tabHeight)),
            ("blockers", ViewPort (0, timingHeight + timingRangeHeight + tabHeight) (300, canvasDim ^. _2))
          ]
          -}
    }
