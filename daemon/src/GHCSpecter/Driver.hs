{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Driver
  ( -- * types
    ClientSession (..),
    HasClientSession (..),

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

data ClientSession = ClientSession
  { _csUIStateRef :: TVar UIState
  , _csServerStateRef :: TVar ServerState
  , _csSubscriberEvent :: TChan Event
  , _csPublisherState :: TChan (UIState, ServerState)
  , _csPublisherBkgEvent :: TChan BackgroundEvent
  }

makeClassy ''ClientSession

main ::
  ClientSession ->
  TChan Bool ->
  IO ()
main cs signalChan = do
  -- start chanDriver
  lastMessageSN <-
    (^. serverMessageSN) <$> atomically (readTVar ssRef)
  _ <- forkIO $ chanDriver lastMessageSN
  -- start controlDriver
  _ <- forkIO $ controlDriver
  pure ()
  where
    uiRef = cs ^. csUIStateRef
    ssRef = cs ^. csServerStateRef
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
              (uiRef, ssRef, chanBkg, signalChan)
          atomically $ do
            ui <- readTVar uiRef
            ss <- readTVar ssRef
            writeTChan chanState (ui, ss)
          pure ec'
