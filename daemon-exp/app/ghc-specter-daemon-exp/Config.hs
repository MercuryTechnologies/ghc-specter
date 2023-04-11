{-# LANGUAGE OverloadedStrings #-}

module Config (
  appWidgetConfig,
) where

import Control.Lens ((^.), _2)
import Data.Map qualified as Map
import GHCSpecter.Graphics.DSL (ViewPort (..))
import GHCSpecter.UI.Constants (
  canvasDim,
  modGraphHeight,
  modGraphWidth,
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
          [ ("main", ViewPort (0, 0) (modGraphWidth, modGraphHeight))
          ]
    , _wcfgSourceView = Map.empty
    , _wcfgTiming =
        Map.fromList
          [ ("timing-chart", ViewPort (0, 0) (0.8 * timingWidth, timingHeight))
          , ("mem-chart", ViewPort (0.8 * timingWidth, 0) (timingWidth, timingHeight))
          , ("timing-range", ViewPort (0, timingHeight) (timingWidth, timingHeight + timingRangeHeight))
          , ("blockers", ViewPort (0, timingHeight + timingRangeHeight) (300, canvasDim ^. _2))
          ]
    }
