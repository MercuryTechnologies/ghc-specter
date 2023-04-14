{-# LANGUAGE OverloadedStrings #-}

module Config (
  appWidgetConfig,
) where

import Control.Lens (to, (^.), _1, _2)
import Data.Map qualified as Map
import GHCSpecter.Graphics.DSL (ViewPort (..))
import GHCSpecter.UI.Constants (
  canvasDim,
  modGraphHeight,
  modGraphWidth,
  tabHeight,
  timingHeight,
  timingRangeHeight,
  timingWidth,
 )
import GHCSpecter.UI.Types (
  WidgetConfig (..),
 )

appWidgetConfig :: WidgetConfig
appWidgetConfig =
  WidgetConfig
    { _wcfgSession =
        Map.fromList
          [ ("tab", ViewPort (0, 0) (canvasDim ^. _1, tabHeight))
          ]
    , _wcfgModuleGraph =
        Map.fromList
          [ ("tab", ViewPort (0, 0) (canvasDim ^. _1, tabHeight))
          , ("main-module-graph", ViewPort (0, tabHeight) (modGraphWidth, 0.5 * modGraphHeight + tabHeight))
          , ("sub-module-graph", ViewPort (0, 0.5 * modGraphHeight + tabHeight) (canvasDim ^. _1, canvasDim ^. _2))
          ]
    , _wcfgSourceView =
        Map.fromList
          [ ("tab", ViewPort (0, 0) (canvasDim ^. _1, tabHeight))
          , ("module-tree", ViewPort (0, tabHeight) (canvasDim ^. _1 . to (* 0.2), canvasDim ^. _2))
          , ("source-view", ViewPort (canvasDim ^. _1 . to (* 0.2), tabHeight) (canvasDim ^. _1 . to (* 0.6), canvasDim ^. _2))
          , ("supple-view", ViewPort (canvasDim ^. _1 . to (* 0.6), tabHeight) (canvasDim ^. _1, canvasDim ^. _2))
          ]
    , _wcfgTiming =
        Map.fromList
          [ ("tab", ViewPort (0, 0) (canvasDim ^. _1, tabHeight))
          , ("timing-chart", ViewPort (0, tabHeight) (0.8 * timingWidth, timingHeight + tabHeight))
          , ("mem-chart", ViewPort (0.8 * timingWidth, tabHeight) (timingWidth, timingHeight + tabHeight))
          , ("timing-range", ViewPort (0, timingHeight + tabHeight) (timingWidth, timingHeight + timingRangeHeight + tabHeight))
          , ("blockers", ViewPort (0, timingHeight + timingRangeHeight + tabHeight) (300, canvasDim ^. _2))
          ]
    }
