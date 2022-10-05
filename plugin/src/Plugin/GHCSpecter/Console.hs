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
  ( TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
    retry,
    writeTVar,
  )
import Control.Concurrent.STM qualified as STM
import Control.Exception (AsyncException, catch)
import Control.Monad (forever, when)
import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List qualified as L
import Data.Maybe (isNothing)
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
  TVar (Maybe (m ())) ->
  IO ()
consoleAction queue drvId loc cmds actionRef = liftIO $ do
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
          case L.lookup ":unqualified" (unCommandSet cmds) of
            Just cmd -> do
              let action = do
                    txt <- cmd
                    liftIO $ consoleMessage txt
              atomically $
                writeTVar actionRef (Just action)
            Nothing ->
              consoleMessage $
                "cannot show unqualified imports at the breakpoint: " <> T.pack (show loc)
        else do
          consoleMessage $
            "cannot show unqualified imports at the breakpoint: " <> T.pack (show loc)
    PrintCore ->
      case loc of
        Core2Core _ -> do
          case L.lookup ":print-core" (unCommandSet cmds) of
            Just cmd -> do
              let action = do
                    txt <- cmd
                    liftIO $ consoleMessage txt
              atomically $
                writeTVar actionRef (Just action)
            Nothing ->
              consoleMessage $
                "cannot print core at the breakpoint: " <> T.pack (show loc)
        _ ->
          consoleMessage $
            "cannot print core at the breakpoint: " <> T.pack (show loc)
  where
    consoleMessage = queueMessage queue . CMConsole drvId

sessionInPause ::
  MonadIO m =>
  MsgQueue ->
  DriverId ->
  BreakpointLoc ->
  CommandSet m ->
  TVar (Maybe (m ())) ->
  IO ()
sessionInPause queue drvId loc cmds actionRef = do
  atomically $ do
    isPaused <- sessionIsPaused . psSessionInfo <$> readTVar sessionRef
    -- prodceed when the session is paused.
    STM.check isPaused
  queueMessage queue (CMPaused drvId (Just loc))
  (forever $ consoleAction queue drvId loc cmds actionRef)
    `catch` (\(_e :: AsyncException) -> queueMessage queue (CMPaused drvId Nothing))

breakPoint ::
  forall m.
  MonadIO m =>
  MsgQueue ->
  DriverId ->
  BreakpointLoc ->
  CommandSet m ->
  m ()
breakPoint queue drvId loc cmds = do
  actionRef <- liftIO $ newTVarIO Nothing
  tid <- liftIO $ forkIO $ sessionInPause queue drvId loc cmds actionRef
  loopM (go actionRef) (pure (), True)
  liftIO $ killThread tid
  where
    go :: TVar (Maybe (m ())) -> (m (), Bool) -> m (Either (m (), Bool) ())
    go actionRef (action, isBlocked) = do
      action
      liftIO $
        atomically $ do
          psess <- readTVar sessionRef
          let sinfo = psSessionInfo psess
              isSessionPaused = sessionIsPaused sinfo
              isDriverInStep =
                maybe False (== drvId)
                  . consoleDriverInStep
                  . psConsoleState
                  $ psess
              isBlocked' = isSessionPaused && not isDriverInStep
          mstagedAction <- readTVar actionRef
          if isNothing mstagedAction && isBlocked == isBlocked'
            then retry
            else case mstagedAction of
              Just stagedAction -> do
                writeTVar actionRef Nothing
                pure $ Left (stagedAction, isBlocked)
              Nothing ->
                -- loop until the session is resumed.
                if isBlocked'
                  then pure $ Left (pure (), isBlocked')
                  else do
                    when (isDriverInStep && isSessionPaused) $ do
                      let console = psConsoleState psess
                          console' = console {consoleDriverInStep = Nothing}
                          psess' = psess {psConsoleState = console'}
                      writeTVar sessionRef psess'
                    pure (Right ())
