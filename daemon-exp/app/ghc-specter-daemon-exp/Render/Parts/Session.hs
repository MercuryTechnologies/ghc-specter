{-# LANGUAGE OverloadedStrings #-}

module Render.Parts.Session (renderSession) where

import Control.Lens (to, (^.))
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import Data.List (partition)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import GHCSpecter.Channel.Outbound.Types (
  SessionInfo (..),
  getEnd,
 )
import GHCSpecter.Data.Map (keyMapToList)
import GHCSpecter.Graphics.DSL (Color (..), Scene (..), ViewPort (..))
import GHCSpecter.Render.Components.Tab (compileTab)
import GHCSpecter.Render.Session (
  compileModuleInProgress,
  compilePauseResume,
  compileSession,
 )
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
 )
import GHCSpecter.UI.Types.Event (Event (..), Tab (..))
import GHCSpecter.Util.Transformation (translateToOrigin)
import GI.Cairo.Render qualified as R
import Render.Util.Rules (boxRules)
import Renderer (
  addEventMap,
  renderScene,
  resetWidget,
  setColor,
 )
import Types (GtkRender)

renderSession ::
  ServerState ->
  SessionUI ->
  GtkRender Event ()
renderSession ss sessui = do
  wcfg <- resetWidget TabSession
  for_ (Map.lookup "tab" wcfg) $ \vpCvs -> do
    let sceneTab = TabEv <$> compileTab topLevelTab (Just TabSession)
        sceneTab' =
          sceneTab
            { sceneGlobalViewPort = vpCvs
            }
    renderScene sceneTab'
    addEventMap sceneTab'
  for_ (Map.lookup "session-main" wcfg) $ \vpCvs -> do
    let vpiMain = sessui ^. sessionUIMainViewPort
        vpMain = fromMaybe (vpiMain ^. vpViewPort) (vpiMain ^. vpTempViewPort)
        sceneMain = compileSession ss
        sceneMain' =
          sceneMain
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = vpMain
            }
    renderScene sceneMain'
    addEventMap sceneMain'
  for_ (Map.lookup "module-status" wcfg) $ \vpCvs -> do
    let ViewPort (cx0, cy0) (cx1, cy1) = vpCvs
    setColor Ivory
    -- TODO: this should be wrapped in a function.
    lift $ do
      R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
      R.fill
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
    renderScene sceneModStatus'
    addEventMap sceneModStatus'
  for_ (Map.lookup "session-button" wcfg) $ \vpCvs -> do
    let sessionInfo = ss ^. serverSessionInfo
        scenePause = SessionEv <$> compilePauseResume sessionInfo
        scenePause' =
          scenePause
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = translateToOrigin vpCvs
            }
    renderScene scenePause'
    addEventMap scenePause'
  when (ss ^. serverSessionInfo . to sessionIsPaused) $ do
    for_ (Map.lookup "console-panel" wcfg) $ \vpCvs -> do
      let ViewPort (cx0, cy0) (cx1, cy1) = vpCvs
      setColor White
      -- TODO: this should be wrapped in a function.
      lift $ do
        R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
        R.fill
      boxRules vpCvs