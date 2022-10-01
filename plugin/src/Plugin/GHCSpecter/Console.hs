module Plugin.GHCSpecter.Console
  ( -- * A list of available commands
    CommandSet (..),
    emptyCommandSet,

    -- * entry point to console when paused
    breakPoint,
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
import Control.Exception (AsyncException, catch)
import Control.Monad (forever, when)
import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Text qualified as T
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
  ( ConsoleState (..),
    MsgQueue (..),
    PluginSession (..),
    sessionRef,
  )

-- | a list of (command name, command action)
newtype CommandSet m = CommandSet {unCommandSet :: [(Text, m Text)]}

emptyCommandSet :: CommandSet m
emptyCommandSet = CommandSet []

consoleAction ::
  MonadIO m =>
  MsgQueue ->
  DriverId ->
  BreakpointLoc ->
  CommandSet m ->
  IO ()
consoleAction queue drvId loc cmds = liftIO $ do
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
      consoleMessage pongMsg
    NextBreakpoint -> do
      putStrLn "NextBreakpoint"
      atomically $
        modifyTVar' sessionRef $ \psess ->
          let console = psConsoleState psess
              console' = console {consoleDriverInStep = Just drvId}
           in psess {psConsoleState = console'}
    ShowUnqualifiedImports ->
      if loc == Typecheck
        then do
          consoleMessage "show unqualified imports not implemented"
        else do
          consoleMessage $
            "cannot show unqualified imports at the breakpoint: " <> T.pack (show loc)
  where
    consoleMessage = queueMessage queue . CMConsole drvId

sessionInPause ::
  MonadIO m =>
  MsgQueue ->
  DriverId ->
  BreakpointLoc ->
  CommandSet m ->
  IO ()
sessionInPause queue drvId loc cmds = do
  atomically $ do
    isPaused <- sessionIsPaused . psSessionInfo <$> readTVar sessionRef
    -- prodceed when the session is paused.
    STM.check isPaused
  queueMessage queue (CMPaused drvId (Just loc))
  (forever $ consoleAction queue drvId loc cmds)
    `catch` (\(_e :: AsyncException) -> queueMessage queue (CMPaused drvId Nothing))

breakPoint ::
  MonadIO m =>
  MsgQueue ->
  DriverId ->
  BreakpointLoc ->
  CommandSet m ->
  m ()
breakPoint queue drvId loc cmds = liftIO $ do
  tid <- forkIO $ sessionInPause queue drvId loc cmds
  loopM go (pure ())
  killThread tid
  where
    go action = do
      action
      atomically $ do
        psess <- readTVar sessionRef
        let sinfo = psSessionInfo psess
            isSessionPaused = sessionIsPaused sinfo
            isDriverInStep =
              maybe False (== drvId)
                . consoleDriverInStep
                . psConsoleState
                $ psess
        -- block until the session is resumed.
        STM.check (not isSessionPaused || isDriverInStep)
        when (isDriverInStep && isSessionPaused) $ do
          let console = psConsoleState psess
              console' = console {consoleDriverInStep = Nothing}
              psess' = psess {psConsoleState = console'}
          writeTVar sessionRef psess'
        pure (Right ())
