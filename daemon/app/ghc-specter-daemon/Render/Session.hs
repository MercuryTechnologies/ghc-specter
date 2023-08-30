{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Session
  ( render,
    renderCompilationStatus,
  )
where

import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.List (partition)
import Data.Maybe (isJust)
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (fromBool, toBool)
import GHCSpecter.Channel.Outbound.Types
  ( SessionInfo (..),
    getEnd,
  )
import GHCSpecter.Data.Map (keyMapToList)
import GHCSpecter.Server.Types
  ( ServerState (..),
    TimingState (..),
  )
import GHCSpecter.UI.Session qualified as Session
import GHCSpecter.UI.Types (UIState (..))
import GHCSpecter.UI.Types.Event
  ( SessionEvent (..),
    UserEvent (..),
  )
import Handler (sendToControl)
import ImGui qualified
import Render.Common (renderComponent)
import STD.Deletable (delete)
import Util.GUI (defTableFlags, windowFlagsNone)
import Util.Render
  ( SharedState (..),
    mkRenderState,
    runImRender,
  )

render :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
render _ui ss = do
  vec1 <- liftIO $ ImGui.newImVec2 0 100
  vec2 <- liftIO $ ImGui.newImVec2 500 0
  vec3 <- liftIO $ ImGui.newImVec2 0 0
  _ <- liftIO $ ImGui.beginChild ("#session-info" :: CString) vec1 (fromBool False) windowFlagsNone
  renderSessionInfo ss
  liftIO ImGui.endChild
  whenM (toBool <$> liftIO (ImGui.beginTable ("##table" :: CString) 2 defTableFlags)) $ do
    liftIO $ ImGui.tableSetupColumn_ ("#session" :: CString)
    liftIO $ ImGui.tableNextRow 0
    liftIO $ ImGui.tableSetColumnIndex 0
    --
    liftIO $ ImGui.textUnformatted ("Process info" :: CString)
    _ <- liftIO $ ImGui.beginChild ("#process-info" :: CString) vec2 (fromBool False) windowFlagsNone
    renderProcessPanel ss
    liftIO ImGui.endChild
    --
    liftIO $ ImGui.tableSetColumnIndex 1
    liftIO $ ImGui.textUnformatted ("RTS info" :: CString)
    _ <- liftIO $ ImGui.beginChild ("#rts-info" :: CString) vec3 (fromBool False) windowFlagsNone
    renderRtsPanel ss
    liftIO ImGui.endChild
    --
    liftIO ImGui.endTable

  liftIO $ delete vec1
  liftIO $ delete vec2
  liftIO $ delete vec3

renderSessionInfo :: ServerState -> ReaderT (SharedState UserEvent) IO ()
renderSessionInfo ss = do
  renderState <- mkRenderState
  runImRender renderState $
    renderComponent False SessionEv (Session.buildSession ss)

renderProcessPanel :: ServerState -> ReaderT (SharedState UserEvent) IO ()
renderProcessPanel ss = do
  renderState <- mkRenderState
  runImRender renderState $
    renderComponent False SessionEv (Session.buildProcessPanel ss)

renderRtsPanel :: ServerState -> ReaderT (SharedState UserEvent) IO ()
renderRtsPanel ss = do
  renderState <- mkRenderState
  runImRender renderState $
    renderComponent False SessionEv (Session.buildRtsPanel ss)

renderCompilationStatus :: ServerState -> ReaderT (SharedState UserEvent) IO ()
renderCompilationStatus ss = do
  shared <- ask
  liftIO $ do
    ImGui.textUnformatted (statusTxt :: CString)
    whenM (toBool <$> ImGui.button (buttonTxt :: CString)) $
      sendToControl shared (SessionEv event)
  renderState <- mkRenderState
  runImRender renderState $
    renderComponent False SessionEv (Session.buildModuleInProgress drvModMap pausedMap timingInProg)
  where
    sessionInfo = ss._serverSessionInfo
    statusTxt
      | sessionInfo.sessionIsPaused = "GHC is paused."
      | otherwise = "GHC is working."
    buttonTxt
      | sessionInfo.sessionIsPaused = "Resume Session"
      | otherwise = "Pause Session"
    event
      | sessionInfo.sessionIsPaused = ResumeSessionEv
      | otherwise = PauseSessionEv

    drvModMap = ss._serverDriverModuleMap
    timing = ss._serverTiming._tsTimingMap
    pausedMap = ss._serverPaused
    timingList = keyMapToList timing
    (_timingDone, timingInProg) =
      partition (\(_, t) -> isJust (getEnd t)) timingList
