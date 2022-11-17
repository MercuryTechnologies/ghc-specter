{-# LANGUAGE LambdaCase #-}

module Plugin.GHCSpecter.Console (
  -- * entry point to console when paused
  breakPoint,
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (
  TVar,
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
import Data.Foldable (for_)
import Data.List qualified as L
import Data.Maybe (isNothing)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHCSpecter.Channel.Common.Types (DriverId)
import GHCSpecter.Channel.Inbound.Types (
  ConsoleRequest (..),
 )
import GHCSpecter.Channel.Outbound.Types (
  BreakpointLoc (..),
  ChanMessage (..),
  ConsoleReply (..),
  SessionInfo (..),
 )
import Plugin.GHCSpecter.Comm (queueMessage)
import Plugin.GHCSpecter.Tasks (CommandSet (..))
import Plugin.GHCSpecter.Types (
  ConsoleState (..),
  MsgQueue (..),
  PluginSession (..),
  getModuleFromDriverId,
  getMsgQueue,
  sessionRef,
 )

consoleAction ::
  MonadIO m =>
  DriverId ->
  BreakpointLoc ->
  CommandSet m ->
  TVar (Maybe (m ())) ->
  IO ()
consoleAction drvId loc cmds actionRef = liftIO $ do
  mqueue <- atomically getMsgQueue
  for_ mqueue $ \queue -> do
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
    let doCommand cmdStr chkLoc cmdDesc args
          | chkLoc loc
          , Just cmd <- L.lookup cmdStr (unCommandSet cmds) = do
              let action = liftIO . reply =<< cmd args
              atomically $
                writeTVar actionRef (Just action)
          | otherwise =
              reply $
                ConsoleReplyText Nothing $
                  "cannot " <> cmdDesc <> " at the breakpoint: " <> T.pack (show loc)
    case req of
      Ping msg -> do
        TIO.putStrLn $ "ping: " <> msg
        let pongMsg = "pong: " <> msg
        reply (ConsoleReplyText Nothing pongMsg)
      NextBreakpoint -> do
        putStrLn "NextBreakpoint"
        atomically $
          modifyTVar' sessionRef $ \psess ->
            let console = psConsoleState psess
                console' = console {consoleDriverInStep = Just drvId}
             in psess {psConsoleState = console'}
      ShowRenamed ->
        doCommand ":show-renamed" (== RenamedResultAction) "show renamed group" []
      ShowSplice ->
        doCommand ":show-splice" (\x -> x == RnSplice) "show splice" []
      ShowExpr ->
        doCommand ":show-expr" (\x -> x == SpliceRunAction || x == PreRunMeta) "show expr" []
      ShowResult ->
        doCommand ":show-result" (\x -> x == PostRunMeta) "show expr" []
      ShowUnqualifiedImports ->
        doCommand ":unqualified" (== TypecheckResultAction) "show unqualified imports" []
      ListCore ->
        doCommand ":list-core" (\case Core2Core _ -> True; _ -> False) "list core" []
      PrintCore args ->
        doCommand ":print-core" (\case Core2Core _ -> True; _ -> False) "print core" args
  where
    reply = queueMessage . CMConsole drvId

sessionInPause ::
  MonadIO m =>
  DriverId ->
  BreakpointLoc ->
  CommandSet m ->
  TVar (Maybe (m ())) ->
  IO ()
sessionInPause drvId loc cmds actionRef = do
  atomically $ do
    isPaused <- sessionIsPaused . psSessionInfo <$> readTVar sessionRef
    -- proceed when the session is paused.
    STM.check isPaused
  queueMessage (CMPaused drvId (Just loc))
  (forever $ consoleAction drvId loc cmds actionRef)
    `catch` (\(_e :: AsyncException) -> queueMessage (CMPaused drvId Nothing))

breakPoint ::
  forall m.
  MonadIO m =>
  DriverId ->
  BreakpointLoc ->
  CommandSet m ->
  m ()
breakPoint drvId loc cmds = do
  actionRef <- liftIO $ newTVarIO Nothing
  tid <- liftIO $ forkIO $ sessionInPause drvId loc cmds actionRef
  loopM (go actionRef) (pure (), True)
  liftIO $ killThread tid
  where
    go :: TVar (Maybe (m ())) -> (m (), Bool) -> m (Either (m (), Bool) ())
    go actionRef (action, isBlocked) = do
      action
      liftIO $ do
        -- TODO: a series of @atomically@ looks fishy. combine them if okay.
        mmodName <- atomically $ getModuleFromDriverId drvId
        atomically $ do
          psess <- readTVar sessionRef
          let modBreakpoints = psModuleBreakpoints psess
              doesHitBreakpoints =
                maybe False (\modName -> modName `elem` modBreakpoints) mmodName
          when doesHitBreakpoints $
            modifyTVar' sessionRef $ \s ->
              let sinfo = psSessionInfo s
                  sinfo' = sinfo {sessionIsPaused = True}
               in s {psSessionInfo = sinfo'}
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
