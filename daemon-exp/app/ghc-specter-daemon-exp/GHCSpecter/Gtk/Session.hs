{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Gtk.Session (renderSession) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Lens ((^.))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Data.Foldable (for_)
import Data.List (partition)
import Data.List qualified as L
import Data.Maybe (isJust)
import GHCSpecter.Channel.Outbound.Types (
  getEnd,
 )
import GHCSpecter.Data.Map (keyMapToList)
import GHCSpecter.Graphics.DSL (
  Color (..),
  Scene (..),
  Stage (..),
  ViewPort (..),
 )
import GHCSpecter.Gtk.Renderer (
  render,
  setColor,
 )
import GHCSpecter.Gtk.Types (GtkRender, ViewBackend (..))
import GHCSpecter.Gtk.Util.Rules (boxRules)
import GHCSpecter.Server.Types (
  HasServerState (..),
  HasTimingState (..),
  ServerState,
 )
import GHCSpecter.UI.Session (
  buildModuleInProgress,
  buildPauseResume,
  buildProcessPanel,
  buildRtsPanel,
  buildSession,
 )
import GHCSpecter.UI.Types.Event (Event (..))
import GI.Cairo.Render qualified as R

renderSession ::
  ServerState ->
  GtkRender Event ()
renderSession ss = do
  stageRef <- vbStage <$> ask
  Stage stage <- R.liftIO $ atomically $ readTVar stageRef
  for_ (L.find ((== "session-main") . sceneId) stage) $ \scene0 -> do
    sceneMain <- buildSession ss
    let sceneMain' =
          sceneMain
            { sceneGlobalViewPort = sceneGlobalViewPort scene0
            , sceneLocalViewPort = sceneLocalViewPort scene0
            }
    render sceneMain'
  for_ (L.find ((== "session-process") . sceneId) stage) $ \scene0 -> do
    boxRules (sceneGlobalViewPort scene0)
    sceneProcess <- buildProcessPanel ss
    let sceneProcess' =
          sceneProcess
            { sceneGlobalViewPort = sceneGlobalViewPort scene0
            , sceneLocalViewPort = sceneLocalViewPort scene0
            }
    render sceneProcess'
  for_ (L.find ((== "session-rts") . sceneId) stage) $ \scene0 -> do
    boxRules (sceneGlobalViewPort scene0)
    sceneRts <- buildRtsPanel ss
    let sceneRts' =
          sceneRts
            { sceneGlobalViewPort = sceneGlobalViewPort scene0
            , sceneLocalViewPort = sceneLocalViewPort scene0
            }
    render sceneRts'
  for_ (L.find ((== "module-status") . sceneId) stage) $ \scene0 -> do
    let ViewPort (cx0, cy0) (cx1, cy1) = sceneGlobalViewPort scene0
    setColor Ivory
    -- TODO: this should be wrapped in a function.
    lift $ do
      R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
      R.fill
    boxRules (sceneGlobalViewPort scene0)
    let drvModMap = ss ^. serverDriverModuleMap
        timing = ss ^. serverTiming . tsTimingMap
        pausedMap = ss ^. serverPaused
        timingList = keyMapToList timing
        (_timingDone, timingInProg) =
          partition (\(_, t) -> isJust (getEnd t)) timingList
    sceneModStatus <- buildModuleInProgress drvModMap pausedMap timingInProg
    let sceneModStatus' =
          sceneModStatus
            { sceneGlobalViewPort = sceneGlobalViewPort scene0
            , sceneLocalViewPort = sceneLocalViewPort scene0
            }
    render sceneModStatus'
  for_ (L.find ((== "session-button") . sceneId) stage) $ \scene0 -> do
    let sessionInfo = ss ^. serverSessionInfo
    scenePause <- fmap (fmap SessionEv) <$> buildPauseResume sessionInfo
    let scenePause' =
          scenePause
            { sceneGlobalViewPort = sceneGlobalViewPort scene0
            , sceneLocalViewPort = sceneLocalViewPort scene0
            }
    render scenePause'
