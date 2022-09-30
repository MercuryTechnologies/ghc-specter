module Plugin.GHCSpecter.Console
  ( breakPoint,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
  ( atomically,
    modifyTVar',
    readTVar,
    retry,
    writeTVar,
  )
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as E
import Control.Monad (forever, when)
import Data.Text.IO qualified as TIO
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Channel.Inbound.Types
  ( ConsoleRequest (..),
  )
import GHCSpecter.Channel.Outbound.Types
  ( BreakpointLoc (..),
    ChanMessage (..),
    SessionInfo (..),
  )
import Plugin.GHCSpecter.Comm (queueMessage)
import Plugin.GHCSpecter.Types
  ( MsgQueue (..),
    PluginSession (..),
    sessionRef,
  )

breakPoint :: MsgQueue -> DriverId -> BreakpointLoc -> IO ()
breakPoint queue drvId loc = do
  tid <- forkIO $ sessionInPause queue drvId loc
  atomically $ do
    psess <- readTVar sessionRef
    let sinfo = psSessionInfo psess
        isSessionPaused = sessionIsPaused sinfo
        isDriverInStep = maybe False (== drvId) $ psDriverInStep psess
    -- block until the session is resumed.
    STM.check (not isSessionPaused || isDriverInStep)
    when (isDriverInStep && isSessionPaused) $ do
      let psess' = psess {psDriverInStep = Nothing}
      writeTVar sessionRef psess'
  killThread tid

consoleAction :: MsgQueue -> DriverId -> IO ()
consoleAction queue drvId = do
  let rQ = msgReceiverQueue queue
  req <-
    atomically $ do
      mreq <- readTVar rQ
      case mreq of
        Nothing -> retry
        Just (drvId', req) ->
          if drvId == drvId'
            then do
              writeTVar rQ Nothing
              pure req
            else retry
  case req of
    Ping msg -> do
      TIO.putStrLn $ "ping: " <> msg
      let pongMsg = "pong: " <> msg
      queueMessage queue (CMConsole drvId pongMsg)
    NextBreakpoint -> do
      putStrLn "NextBreakpoint"
      atomically $
        modifyTVar' sessionRef $ \psess -> psess {psDriverInStep = Just drvId}

sessionInPause :: MsgQueue -> DriverId -> BreakpointLoc -> IO ()
sessionInPause queue drvId loc = do
  atomically $ do
    isPaused <- sessionIsPaused . psSessionInfo <$> readTVar sessionRef
    -- prodceed when the session is paused.
    STM.check isPaused
  queueMessage queue (CMPaused drvId (Just loc))
  (forever $ consoleAction queue drvId)
    `E.catch` (\(_e :: E.AsyncException) -> queueMessage queue (CMPaused drvId Nothing))
