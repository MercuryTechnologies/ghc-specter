module GHCSpecter.Driver.Session
  ( -- * main session procedure
    main,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
  ( atomically,
    readTQueue,
    readTVar,
    retry,
    writeTChan,
    writeTQueue,
  )
import Control.Lens ((^.))
import Control.Monad.Extra (loopM)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Time.Clock (nominalDiffTimeToSeconds)
import GHCSpecter.Control.Runner
  ( RunnerEnv (..),
    stepControlUpToEvent,
  )
import GHCSpecter.Control.Types (Control)
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
    Event (SysEv),
    SystemEvent (BkgEv),
  )

main ::
  RunnerEnv e ->
  ServerSession ->
  ClientSession ->
  Control e () ->
  IO ()
main runner servSess cs controlMain = do
  -- start chanDriver
  lastMessageSN <-
    (^. serverMessageSN) <$> atomically (readTVar ssRef)
  _ <- forkIO $ chanDriver lastMessageSN
  -- start controlDriver
  _ <- forkIO $ controlDriver runner
  pure ()
  where
    ssRef = servSess ^. ssServerStateRef
    uiRef = cs ^. csUIStateRef
    -- chanEv = cs ^. csSubscriberEvent
    chanState = cs ^. csPublisherState
    chanQEv = cs ^. csPublisherEvent
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
          ev <- atomically $ readTQueue chanQEv
          ec' <-
            runReaderT (stepControlUpToEvent ev c) runner'
          atomically $ do
            ui <- readTVar uiRef
            ss <- readTVar ssRef
            writeTChan chanState (ui, ss)
          pure ec'
