module GHCSpecter.Driver.Session
  ( -- * main session procedure
    main,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
  ( atomically,
    readTChan,
    readTVar,
    retry,
    writeTChan,
  )
import Control.Lens ((^.))
import Control.Monad.Extra (loopM)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Time.Clock (nominalDiffTimeToSeconds)
import GHCSpecter.Control qualified as Control (main)
import GHCSpecter.Control.Runner (stepControlUpToEvent)
import GHCSpecter.Driver.Session.Types
  ( ClientSession (..),
    HasClientSession (..),
    HasServerSession (..),
    ServerSession (..),
  )
import GHCSpecter.Server.Types (HasServerState (..))
import GHCSpecter.UI.Constants (chanUpdateInterval)
import GHCSpecter.UI.Types.Event
  ( BackgroundEvent (MessageChanUpdated),
  )

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
