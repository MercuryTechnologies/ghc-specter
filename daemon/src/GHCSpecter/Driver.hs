{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Driver
  ( -- * session types for daemon
    ServerSession (..),
    HasServerSession (..),
    ClientSession (..),
    HasClientSession (..),

    -- * UI Channel
    UIChannel (..),
    HasUIChannel (..),

    -- * main driver
    main,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
  ( TChan,
    TVar,
    atomically,
    readTChan,
    readTVar,
    retry,
    writeTChan,
  )
import Control.Lens (makeClassy, (^.))
import Control.Monad.Extra (loopM)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Time.Clock (nominalDiffTimeToSeconds)
import GHCSpecter.Control qualified as Control (main)
import GHCSpecter.Control.Runner (stepControlUpToEvent)
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState (..),
  )
import GHCSpecter.UI.Constants (chanUpdateInterval)
import GHCSpecter.UI.Types (UIState)
import GHCSpecter.UI.Types.Event
  ( BackgroundEvent (MessageChanUpdated),
    Event,
  )

-- Session = State + Channel

data ServerSession = ServerSession
  { _ssServerStateRef :: TVar ServerState
  , _ssSubscriberSignal :: TChan Bool
  }

makeClassy ''ServerSession

data ClientSession = ClientSession
  { _csUIStateRef :: TVar UIState
  , _csSubscriberEvent :: TChan Event
  , _csPublisherState :: TChan (UIState, ServerState)
  , _csPublisherBkgEvent :: TChan BackgroundEvent
  }

makeClassy ''ClientSession

-- | communication channel that UI renderer needs
-- Note that subscribe/publish is named according to UI side semantics.
data UIChannel = UIChannel
  { uiPublisherEvent :: TChan Event
  -- ^ channel for sending event to control
  , uiSubscriberState :: TChan (UIState, ServerState)
  -- ^ channel for receiving state from control
  , uiSubscriberBkgEvent :: TChan BackgroundEvent
  -- ^ channel for receiving background event
  }

makeClassy ''UIChannel

main ::
  ServerSession ->
  ClientSession ->
  IO ()
main servSess cs = do
  -- start chanDriver
  lastMessageSN <-
    (^. serverMessageSN) <$> atomically (readTVar ssRef)
  _ <- forkIO $ chanDriver lastMessageSN
  -- start controlDriver
  _ <- forkIO $ controlDriver
  pure ()
  where
    chanSignal = servSess ^. ssSubscriberSignal
    ssRef = servSess ^. ssServerStateRef

    uiRef = cs ^. csUIStateRef
    chanEv = cs ^. csSubscriberEvent
    chanState = cs ^. csPublisherState
    chanBkg = cs ^. csPublisherBkgEvent
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
        writeTChan chanBkg MessageChanUpdated
      chanDriver newMessageSN

    -- connector between driver and Control frame
    controlDriver = loopM step (\_ -> Control.main)
      where
        step c = do
          ev <- atomically $ readTChan chanEv
          ec' <-
            runReaderT
              (stepControlUpToEvent ev c)
              (uiRef, ssRef, chanBkg, chanSignal)
          atomically $ do
            ui <- readTVar uiRef
            ss <- readTVar ssRef
            writeTChan chanState (ui, ss)
          pure ec'
