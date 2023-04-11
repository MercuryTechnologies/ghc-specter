{-# LANGUAGE OverloadedStrings #-}

module Config (
  appWidgetConfig,
) where

import Control.Lens ((^.), _1, _2)
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
    { _wcfgSession = Map.empty
    , _wcfgModuleGraph =
        Map.fromList
          [ ("tab", ViewPort (0, 0) (canvasDim ^. _1, tabHeight))
          , ("main-module-graph", ViewPort (0, tabHeight) (modGraphWidth, modGraphHeight + tabHeight))
          ]
    , _wcfgSourceView = Map.empty
    , _wcfgTiming =
        Map.fromList
          [ ("tab", ViewPort (0, 0) (canvasDim ^. _1, tabHeight))
          , ("timing-chart", ViewPort (0, tabHeight) (0.8 * timingWidth, timingHeight + tabHeight))
          , ("mem-chart", ViewPort (0.8 * timingWidth, tabHeight) (timingWidth, timingHeight + tabHeight))
          , ("timing-range", ViewPort (0, timingHeight + tabHeight) (timingWidth, timingHeight + timingRangeHeight + tabHeight))
          , ("blockers", ViewPort (0, timingHeight + timingRangeHeight + tabHeight) (300, canvasDim ^. _2))
          ]
    }