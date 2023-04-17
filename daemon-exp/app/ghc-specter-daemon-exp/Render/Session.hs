{-# LANGUAGE OverloadedStrings #-}

module Render.Session (renderSession) where

import Control.Concurrent.STM (TVar)
import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.List (partition)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import GHCSpecter.Channel.Outbound.Types (getEnd)
import GHCSpecter.Data.Map (keyMapToList)
import GHCSpecter.Graphics.DSL (Scene (..))
import GHCSpecter.Render.Components.Tab (compileTab)
import GHCSpecter.Render.Session (compileModuleInProgress)
import GHCSpecter.Render.Tab (topLevelTab)
import GHCSpecter.Server.Types (
  HasServerState (..),
  HasTimingState (..),
  ServerState,
 )
import GHCSpecter.UI.Types (
  HasSessionUI (..),
  HasViewPortInfo (..),
  SessionUI,
  UIState,
 )
import GHCSpecter.UI.Types.Event (Tab (..))
import GI.Cairo.Render qualified as R
import Render.Common (boxRules)
import Renderer (
  addEventMap,
  renderScene,
  resetWidget,
 )
import Types (ViewBackend)

-- TODO: TVar UIState should be unidirectional channel (only writing should be possible)
--       At minimum, wrap it in ReaderT monad.
renderSession ::
  TVar UIState ->
  ViewBackend ->
  ServerState ->
  SessionUI ->
  R.Render ()
renderSession uiRef vb ss sessui = do
  wcfg <- R.liftIO $ resetWidget uiRef
  for_ (Map.lookup "tab" wcfg) $ \vpCvs -> do
    let sceneTab = compileTab topLevelTab (Just TabSession)
        sceneTab' =
          sceneTab
            { sceneGlobalViewPort = vpCvs
            }
    renderScene vb sceneTab'
    R.liftIO $ addEventMap uiRef sceneTab
  for_ (Map.lookup "module-status" wcfg) $ \vpCvs -> do
    boxRules vpCvs
    let vpiStatus = sessui ^. sessionUIModStatusViewPort
        vpStatus = fromMaybe (vpiStatus ^. vpViewPort) (vpiStatus ^. vpTempViewPort)
        drvModMap = ss ^. serverDriverModuleMap
        timing = ss ^. serverTiming . tsTimingMap
        pausedMap = ss ^. serverPaused
        timingList = keyMapToList timing
        (_timingDone, timingInProg) =
          partition (\(_, t) -> isJust (getEnd t)) timingList
        sceneModStatus = compileModuleInProgress drvModMap pausedMap timingInProg
        sceneModStatus' =
          sceneModStatus
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = vpStatus
            }
    renderScene vb sceneModStatus'
    R.liftIO $ addEventMap uiRef sceneModStatus'
