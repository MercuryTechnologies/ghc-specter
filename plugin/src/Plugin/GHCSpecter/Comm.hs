module Plugin.GHCSpecter.Comm
  ( runMessageQueue,
    queueMessage,
  )
where

import Control.Concurrent (forkIO)
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
import GHC.Driver.Plugins (type CommandLineOption)
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
import GHCSpecter.Config
  ( Config (..),
    defaultGhcSpecterConfigFile,
    loadConfig,
  )
import Network.Socket (Socket)
import Plugin.GHCSpecter.Types
  ( MsgQueue (..),
    PluginSession (..),
    sessionRef,
  )
import System.Directory (doesFileExist)

runMessageQueue :: [CommandLineOption] -> MsgQueue -> IO ()
runMessageQueue opts queue = do
  mipcfile <-
    case opts of
      ipcfile : _ -> pure (Just ipcfile)
      [] -> do
        ecfg <- loadConfig defaultGhcSpecterConfigFile
        case ecfg of
          Left _ -> pure Nothing
          Right cfg -> pure (Just (configSocket cfg))
  for_ mipcfile $ \ipcfile -> do
    socketExists <- doesFileExist ipcfile
    when socketExists $
      runClient ipcfile $ \sock -> do
        _ <- forkIO $ receiver sock
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
    receiver sock = forever $ do
      putStrLn "################"
      putStrLn "receiver started"
      putStrLn "################"
      msg :: Request <- receiveObject sock
      putStrLn "################"
      putStrLn $ "message received: " ++ show msg
      putStrLn "################"
      atomically $
        case msg of
          SessionReq sreq ->
            modifyTVar' sessionRef $ \s ->
              let isPaused
                    | sreq == Pause = True
                    | otherwise = False
                  sinfo = psSessionInfo s
                  sinfo' = sinfo {sessionIsPaused = isPaused}
               in s {psSessionInfo = sinfo'}
          ConsoleReq drvId' creq ->
            writeTVar (msgReceiverQueue queue) (Just (drvId', creq))

queueMessage :: MsgQueue -> ChanMessage a -> IO ()
queueMessage queue !msg =
  atomically $
    modifyTVar' (msgSenderQueue queue) (|> CMBox msg)
