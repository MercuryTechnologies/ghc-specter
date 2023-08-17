{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Driver.Web
  ( webServer,
  )
where

import Concur.Core (Widget, liftSTM, unsafeBlockingIO)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
  ( TChan,
    TQueue,
    TVar,
    atomically,
    newTChanIO,
    newTQueueIO,
    newTVarIO,
    readTChan,
    readTQueue,
    readTVar,
    retry,
    writeTChan,
    writeTQueue,
  )
import Control.Lens (makeClassy, (.~), (^.))
import Control.Monad.Extra (loopM)
import Control.Monad.Trans.Reader (runReaderT)
import Data.IORef (newIORef)
import Data.Time.Clock
  ( getCurrentTime,
    nominalDiffTimeToSeconds,
  )
import GHCSpecter.ConcurReplica.Run (runDefaultWithStyle)
import GHCSpecter.ConcurReplica.Types
  ( IHTML,
    blockDOMUpdate,
    unblockDOMUpdate,
  )
import GHCSpecter.Config (Config (..))
import GHCSpecter.Control qualified as Control (main)
import GHCSpecter.Control.Runner
  ( RunnerEnv (..),
    RunnerHandler (..),
    stepControlUpToEvent,
  )
import GHCSpecter.Control.Types (Control)
import GHCSpecter.Data.Assets qualified as Assets
import GHCSpecter.Driver.Session.Types
  ( HasServerSession (..),
    ServerSession (..),
  )
import GHCSpecter.Server.Types (HasServerState (..), ServerState)
import GHCSpecter.UI.Constants (chanUpdateInterval)
import GHCSpecter.UI.Types
  ( HasUIModel (..),
    HasUIState (..),
    UIState,
    emptyUIState,
  )
import GHCSpecter.UI.Types.Event
  ( BackgroundEvent (..),
    Event (..),
    SystemEvent (BkgEv),
  )
import GHCSpecter.Web (render)

--
-- the following is only needed for web (concur-replica)
--

-- TODO: move this to web

data ClientSessionWeb = ClientSessionWeb
  { _csWebUIStateRef :: TVar UIState,
    _csWebSubscriberEvent :: TChan Event,
    _csWebPublisherState :: TChan (UIState, ServerState),
    _csWebPublisherEvent :: TQueue Event
  }

makeClassy ''ClientSessionWeb

-- | communication channel that UI renderer needs
-- Note that subscribe/publish is named according to UI side semantics.
data UIChannelWeb = UIChannelWeb
  { -- | channel for sending event to control
    uiWebPublisherEvent :: TChan Event,
    -- | channel for receiving state from control
    uiWebSubscriberState :: TChan (UIState, ServerState),
    -- | channel for receiving background event
    uiWebSubscriberEvent :: TQueue Event
  }

makeClassy ''UIChannelWeb

mainWeb ::
  RunnerEnv e ->
  ServerSession ->
  ClientSessionWeb ->
  Control e () ->
  IO ()
mainWeb runner servSess cs controlMain = do
  -- start chanDriver
  lastMessageSN <-
    (^. serverMessageSN) <$> atomically (readTVar ssRef)
  _ <- forkIO $ chanDriver lastMessageSN
  -- start controlDriver
  _ <- forkIO $ controlDriver runner
  pure ()
  where
    ssRef = servSess ^. ssServerStateRef
    uiRef = cs ^. csWebUIStateRef
    chanEv = cs ^. csWebSubscriberEvent
    chanState = cs ^. csWebPublisherState
    chanQEv = cs ^. csWebPublisherEvent
    blockUntilNewMessage lastSN = do
      ss <- readTVar ssRef
      if (ss ^. serverMessageSN == lastSN)
        then retry
        else pure (ss ^. serverMessageSN)

    -- background connector between server channel and UI frame
    chanDriver lastMessageSN = do
      -- wait for next poll
      threadDelay (floor (nominalDiffTimeToSeconds chanUpdateInterval * 1_000_000))
      -- blocked until a new message comes
      newMessageSN <-
        atomically $
          blockUntilNewMessage lastMessageSN
      atomically $
        writeTQueue chanQEv (SysEv (BkgEv MessageChanUpdated))
      chanDriver newMessageSN

    -- connector between driver and Control frame
    controlDriver runner' = loopM step (\_ -> controlMain)
      where
        step c = do
          ev <- atomically $ readTChan chanEv
          ec' <-
            runReaderT (stepControlUpToEvent ev c) runner'
          atomically $ do
            ui <- readTVar uiRef
            ss <- readTVar ssRef
            writeTChan chanState (ui, ss)
          pure ec'

-- NOTE:
-- server state: shared across the session
-- ui state: per web view.
-- control: per web view

webServer :: Config -> ServerSession -> IO ()
webServer cfg servSess = do
  let port = configWebPort cfg
  assets <- Assets.loadAssets
  let styleText = assets ^. Assets.assetsGhcSpecterCss
  runDefaultWithStyle port "ghc-specter" styleText $
    \_ -> do
      uiRef <-
        unsafeBlockingIO $ do
          initTime <- getCurrentTime
          let ui0 = emptyUIState assets initTime
              ui0' = (uiModel . modelTransientBanner .~ Nothing) ui0
          newTVarIO ui0'
      chanEv <- unsafeBlockingIO newTChanIO
      chanState <- unsafeBlockingIO newTChanIO
      chanQEv <- unsafeBlockingIO newTQueueIO

      let cliSess = ClientSessionWeb uiRef chanEv chanState chanQEv
          newUIChan = UIChannelWeb chanEv chanState chanQEv
      -- prepare runner
      -- TODO: make common initialization function (but backend-dep)
      counterRef <- unsafeBlockingIO $ newIORef 0
      let runHandler =
            RunnerHandler
              { runHandlerRefreshAction = pure (),
                runHandlerHitScene = \_ -> pure Nothing,
                runHandlerGetScene = \_ -> pure Nothing,
                runHandlerAddToStage = \_ -> pure ()
              }
          runner =
            RunnerEnv
              { runnerCounter = counterRef,
                runnerUIState = cliSess ^. csWebUIStateRef,
                runnerServerState = servSess ^. ssServerStateRef,
                runnerQEvent = cliSess ^. csWebPublisherEvent,
                runnerSignalChan = servSess ^. ssSubscriberSignal,
                runnerHandler = runHandler
              }
      unsafeBlockingIO $ mainWeb runner servSess cliSess Control.main
      loopM (step newUIChan) (SysEv (BkgEv RefreshUI))
  where
    -- A single step of the outer loop (See Note [Control Loops]).
    step ::
      -- UI comm channel
      UIChannelWeb ->
      -- last event
      Event ->
      Widget IHTML (Either Event ())
    step (UIChannelWeb chanEv chanState chanQEv) ev = do
      (ui, ss) <-
        unsafeBlockingIO $ do
          atomically $ writeTChan chanEv ev
          (ui, ss) <- atomically $ readTChan chanState
          pure (ui, ss)
      stepRender (ui, ss) <|> (Left <$> waitForBkgEv chanQEv)

    stepRender :: (UIState, ServerState) -> Widget IHTML (Either Event ())
    stepRender (ui, ss) = do
      let renderUI =
            if ui ^. uiShouldUpdate
              then unblockDOMUpdate (render (ui, ss))
              else blockDOMUpdate (render (ui, ss))
      Left <$> renderUI

    waitForBkgEv ::
      -- queue for receiving event in other channel
      TQueue Event ->
      Widget IHTML Event
    waitForBkgEv chanQEv = liftSTM $ readTQueue chanQEv
