{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.UI.Tab (
  topLevelTab,
) where

import Control.Lens ((^.), _1)
import GHCSpecter.UI.Components.Tab (TabConfig (..))
import GHCSpecter.UI.Constants (canvasDim, tabHeight)
import GHCSpecter.UI.Types.Event (Tab (..))

topLevelTab :: TabConfig Tab
topLevelTab =
  TabConfig
    { tabCfgId = "tab"
    , tabCfgWidth = canvasDim ^. _1
    , tabCfgHeight = tabHeight
    , tabCfgItems =
        [ (TabSession, "Session")
        , (TabModuleGraph, "Module Graph")
        , (TabSourceView, "Source View")
        , (TabTiming, "Timing")
        ]
    }
