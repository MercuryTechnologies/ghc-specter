{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -w #-}

module GHCSpecter.Util.Dump (dumpTiming) where

import Data.Functor.Identity (runIdentity)
import GHCSpecter.Channel.Outbound.Types
  ( SessionInfo (..),
  )
import GHCSpecter.Data.Timing.Types
  ( TimingTable (..),
  )
import GHCSpecter.Server.Types
  ( ServerState (..),
    TimingState (..),
  )
import GHCSpecter.UI.Components.TimingView qualified as TimingView
import GHCSpecter.UI.Types
  ( UIModel (..),
    UIState (..),
  )

dumpTiming :: UIState -> ServerState -> String
dumpTiming ui ss =
  let drvModMap = ss._serverDriverModuleMap
      tui = ui._uiModel._modelTiming
      ttable = ss._serverTiming._tsTimingTable
      scene = runIdentity $ TimingView.buildTimingChart drvModMap tui ttable
   in show scene
