{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Session
  ( render,
    renderCompilationStatus,
  )
where

import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT, get)
import Data.List (partition)
import Data.Maybe (isJust)
import Data.Text.Foreign qualified as T
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (fromBool, toBool)
import GHCSpecter.Channel.Outbound.Types
  ( DynFlagsInfo (..),
    SessionInfo (..),
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
import STD.Deletable (delete)
import Util.GUI (defTableFlags, windowFlagsNone)
import Util.Render (SharedState (..))

render :: UIState -> ServerState -> StateT (SharedState UserEvent) IO ()
render _ui ss = do
  vec1 <- liftIO $ ImGui.newImVec2 0 300
  vec2 <- liftIO $ ImGui.newImVec2 0 0
  vec3 <- liftIO $ ImGui.newImVec2 0 0
  whenM (toBool <$> liftIO (ImGui.beginTable ("##table" :: CString) 2 defTableFlags)) $ do
    liftIO $ ImGui.tableSetupColumn_ ("#session" :: CString)
    liftIO $ ImGui.tableNextRow 0
    liftIO $ ImGui.tableSetColumnIndex 0

    _ <- liftIO $ ImGui.beginChild ("#session-info" :: CString) vec1 (fromBool False) windowFlagsNone
    renderSessionInfo ss
    liftIO ImGui.endChild

    liftIO $ ImGui.tableSetColumnIndex 1
    _ <- liftIO $ ImGui.beginChild ("#dynflags" :: CString) vec1 (fromBool False) windowFlagsNone
    renderDynFlags ss
    liftIO ImGui.endChild

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

renderSessionInfo :: ServerState -> StateT (SharedState UserEvent) IO ()
renderSessionInfo ss =
  liftIO $
    T.withCString (Session.buildSession ss) $ \cstr ->
      ImGui.textUnformatted cstr

renderDynFlags :: ServerState -> StateT (SharedState UserEvent) IO ()
renderDynFlags ss =
  liftIO $
    T.withCString txt $ \cstr ->
      ImGui.textUnformatted cstr
  where
    txt = dynFlags_info
    dynFlags_info =
      maybe "" unDynFlagsInfo ss._serverSessionInfo.sessionDynFlags

renderProcessPanel :: ServerState -> StateT (SharedState UserEvent) IO ()
renderProcessPanel ss =
  liftIO $
    T.withCString (Session.buildProcessPanel ss) $ \cstr ->
      ImGui.textUnformatted cstr

renderRtsPanel :: ServerState -> StateT (SharedState UserEvent) IO ()
renderRtsPanel ss =
  liftIO $
    T.withCString (Session.buildRtsPanel ss) $ \cstr ->
      ImGui.textUnformatted cstr

renderCompilationStatus :: ServerState -> StateT (SharedState UserEvent) IO ()
renderCompilationStatus ss = do
  shared <- get
  liftIO $ do
    ImGui.textUnformatted (statusTxt :: CString)
    whenM (toBool <$> ImGui.button (buttonTxt :: CString)) $
      sendToControl shared (SessionEv event)
    let txt = Session.buildModuleInProgress drvModMap pausedMap timingInProg
    T.withCString txt $ \cstr ->
      ImGui.textUnformatted cstr
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
