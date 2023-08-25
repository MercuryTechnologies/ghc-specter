{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Session
  ( renderSession,
    renderModuleInProgress,
  )
where

import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.List (partition)
import Data.Maybe (isJust)
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (fromBool, toBool)
import GHCSpecter.Channel.Outbound.Types (getEnd)
import GHCSpecter.Data.Map (keyMapToList)
import GHCSpecter.Server.Types
  ( ServerState (..),
    TimingState (..),
  )
import GHCSpecter.UI.Session qualified as Session
import GHCSpecter.UI.Types (UIState (..))
import GHCSpecter.UI.Types.Event (UserEvent (..))
import ImGui
import Render.Common (renderComponent)
import STD.Deletable (delete)
import Util.GUI (defTableFlags, windowFlagsNone, windowFlagsScroll)
import Util.Render
  ( SharedState (..),
    mkRenderState,
    runImRender,
  )

renderSession :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderSession _ui ss = do
  vec1 <- liftIO $ newImVec2 0 100
  vec2 <- liftIO $ newImVec2 500 0
  vec3 <- liftIO $ newImVec2 0 0
  _ <- liftIO $ beginChild ("#session-info" :: CString) vec1 (fromBool False) windowFlagsNone
  renderSessionButton ss
  renderSessionInfo ss
  liftIO endChild
  whenM (toBool <$> liftIO (beginTable ("##table" :: CString) 2 defTableFlags)) $ do
    liftIO $ tableSetupColumn_ ("#session" :: CString)
    liftIO $ tableNextRow 0
    liftIO $ tableSetColumnIndex 0
    --
    liftIO $ textUnformatted ("Process info" :: CString)
    _ <- liftIO $ beginChild ("#process-info" :: CString) vec2 (fromBool False) windowFlagsNone
    renderProcessPanel ss
    liftIO endChild
    --
    liftIO $ tableSetColumnIndex 1
    liftIO $ textUnformatted ("RTS info" :: CString)
    _ <- liftIO $ beginChild ("#rts-info" :: CString) vec3 (fromBool False) windowFlagsNone
    renderRtsPanel ss
    liftIO endChild
    --
    liftIO endTable

  liftIO $ delete vec1
  liftIO $ delete vec2
  liftIO $ delete vec3

renderSessionInfo :: ServerState -> ReaderT (SharedState UserEvent) IO ()
renderSessionInfo ss = do
  renderState <- mkRenderState
  liftIO $
    runImRender renderState $
      renderComponent SessionEv (Session.buildSession ss)

renderSessionButton :: ServerState -> ReaderT (SharedState UserEvent) IO ()
renderSessionButton ss = do
  renderState <- mkRenderState
  liftIO $
    runImRender renderState $
      renderComponent SessionEv (Session.buildPauseResume sessionInfo)
  where
    sessionInfo = ss._serverSessionInfo

renderProcessPanel :: ServerState -> ReaderT (SharedState UserEvent) IO ()
renderProcessPanel ss = do
  renderState <- mkRenderState
  liftIO $
    runImRender renderState $
      renderComponent SessionEv (Session.buildProcessPanel ss)

renderRtsPanel :: ServerState -> ReaderT (SharedState UserEvent) IO ()
renderRtsPanel ss = do
  renderState <- mkRenderState
  liftIO $
    runImRender renderState $
      renderComponent SessionEv (Session.buildRtsPanel ss)

renderModuleInProgress :: ServerState -> ReaderT (SharedState UserEvent) IO ()
renderModuleInProgress ss = do
  renderState <- mkRenderState
  liftIO $ do
    runImRender renderState $
      renderComponent SessionEv (Session.buildModuleInProgress drvModMap pausedMap timingInProg)
  where
    drvModMap = ss._serverDriverModuleMap
    timing = ss._serverTiming._tsTimingMap
    pausedMap = ss._serverPaused
    timingList = keyMapToList timing
    (_timingDone, timingInProg) =
      partition (\(_, t) -> isJust (getEnd t)) timingList
