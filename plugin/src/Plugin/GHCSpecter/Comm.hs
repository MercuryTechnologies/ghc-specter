module Plugin.GHCSpecter.Comm
  ( runMessageQueue,
    queueMessage,
  )
where

import Control.Concurrent (forkOS)
import Control.Concurrent.STM
  ( atomically,
    modifyTVar',
    readTVar,
    retry,
    writeTVar,
  )
import Control.Monad (forever, when)
import Data.Foldable (for_)
import Data.Foldable qualified as F
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq
import GHCSpecter.Channel.Inbound.Types
  ( Request (..),
    SessionRequest (..),
  )
import GHCSpecter.Channel.Outbound.Types
  ( ChanMessage (..),
    ChanMessageBox (..),
    SessionInfo (..),
  )
import GHCSpecter.Comm
  ( receiveObject,
    runClient,
    sendObject,
  )
import GHCSpecter.Config (Config (..))
import Network.Socket (Socket)
import Plugin.GHCSpecter.Types
  ( MsgQueue (..),
    PluginSession (..),
    getMsgQueue,
    sessionRef,
  )
import System.Directory (doesFileExist)

runMessageQueue :: Config -> MsgQueue -> IO ()
runMessageQueue cfg queue = do
  let mipcfile
        | null (configSocket cfg) = Nothing
        | otherwise = Just (configSocket cfg)
  for_ mipcfile $ \ipcfile -> do
    socketExists <- doesFileExist ipcfile
    when socketExists $
      runClient ipcfile $ \sock -> do
        _ <- forkOS $ receiver sock
        sender sock
  where
    sender :: Socket -> IO ()
    sender sock = forever $ do
      msgs <- atomically $ do
        queued <- readTVar (msgSenderQueue queue)
        if Seq.null queued
          then retry
          else do
            writeTVar (msgSenderQueue queue) Seq.empty
            pure queued
      let msgList = F.toList msgs
      msgList `seq` sendObject sock msgList
    receiver :: Socket -> IO ()
    receiver sock = do
      putStrLn "################"
      putStrLn "receiver started"
      putStrLn "################"
      forever $ do
        msg :: Request <- receiveObject sock
        putStrLn "################"
        putStrLn $ "message received: " ++ show msg
        putStrLn "################"
        case msg of
          SessionReq Pause ->
            atomically $
              modifyTVar' sessionRef $ \s ->
                let sinfo = psSessionInfo s
                    sinfo' = sinfo {sessionIsPaused = True}
                 in s {psSessionInfo = sinfo'}
          SessionReq Resume ->
            atomically $
              modifyTVar' sessionRef $ \s ->
                let sinfo = psSessionInfo s
                    sinfo' = sinfo {sessionIsPaused = False}
                 in s {psSessionInfo = sinfo'}
          SessionReq (SetModuleBreakpoints mods) ->
            atomically $
              modifyTVar' sessionRef $ \s ->
                s {psModuleBreakpoints = mods}
          SessionReq ExitGhcDebug ->
            atomically $
              modifyTVar' sessionRef $ \s ->
                s {psIsInGhcDebug = False}
          ConsoleReq drvId' creq ->
            atomically $
              writeTVar (msgReceiverQueue queue) (Just (drvId', creq))

queueMessage :: ChanMessage a -> IO ()
queueMessage !msg = do
  mqueue <- atomically getMsgQueue
  for_ mqueue $ \queue ->
    atomically $
      modifyTVar' (msgSenderQueue queue) (|> CMBox msg)
