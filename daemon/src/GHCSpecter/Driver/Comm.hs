{-# LANGUAGE GADTs #-}

module GHCSpecter.Driver.Comm
  ( listener,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( TQueue,
    atomically,
    modifyTVar',
    readTChan,
    readTVar,
  )
import Control.Lens ((%~), (.~), (^.))
import Control.Monad (forever, void)
import Data.Foldable qualified as F
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Tree (drawForest)
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Channel.Outbound.Types
  ( ChanMessage (..),
    ChanMessageBox (..),
    Channel (..),
    ConsoleReply (..),
    HsSourceInfo (..),
    SessionInfo (..),
    Timer (..),
  )
import GHCSpecter.Comm
  ( receiveObject,
    runServer,
    sendObject,
  )
import GHCSpecter.Driver.Session.Types
  ( HasServerSession (..),
    ServerSession (..),
  )
import GHCSpecter.Server.Types
  ( ConsoleItem (..),
    HasServerState (..),
    ServerState (..),
    incrementSN,
  )
import GHCSpecter.Util.Map
  ( alterToKeyMap,
    forwardLookup,
    insertToBiKeyMap,
  )
import GHCSpecter.Worker.Hie (hieWorker)
import GHCSpecter.Worker.ModuleGraph (moduleGraphWorker)

updateInbox :: ChanMessageBox -> ServerState -> ServerState
updateInbox chanMsg = incrementSN . updater
  where
    appendConsoleMsg :: ConsoleItem -> Maybe [ConsoleItem] -> Maybe [ConsoleItem]
    appendConsoleMsg newMsg Nothing = Just [newMsg]
    appendConsoleMsg newMsg (Just prevMsgs) = Just (prevMsgs ++ [newMsg])

    updater = case chanMsg of
      CMBox (CMCheckImports modu msg) ->
        (serverInbox %~ M.insert (CheckImports, modu) msg)
      CMBox (CMModuleInfo drvId modu) ->
        let msg = ConsoleItem ("module name: " <> modu)
         in (serverDriverModuleMap %~ insertToBiKeyMap (drvId, modu))
              . (serverConsole %~ alterToKeyMap (appendConsoleMsg msg) drvId)
      CMBox (CMTiming drvId timer') ->
        let f Nothing = Just timer'
            f (Just timer0) =
              Just $ Timer (unTimer timer0 ++ unTimer timer')
         in (serverTiming %~ alterToKeyMap f drvId)
      CMBox (CMSession s') ->
        (serverSessionInfo .~ s')
      CMBox (CMHsSource _modu _info) ->
        id
      CMBox (CMPaused drvId mloc) ->
        let formatMsg (Just loc) = ConsoleItem ("paused at " <> T.pack (show loc))
            formatMsg Nothing = ConsoleItem "resume"
         in (serverPaused %~ alterToKeyMap (const mloc) drvId)
              . (serverConsole %~ alterToKeyMap (appendConsoleMsg (formatMsg mloc)) drvId)
      CMBox (CMConsole drvId creply) ->
        case creply of
          ConsoleReplyText txt ->
            let msg = ConsoleItem txt
             in (serverConsole %~ alterToKeyMap (appendConsoleMsg msg) drvId)
          ConsoleReplyCore forest ->
            let msg = ConsoleItem (T.pack . drawForest . fmap (fmap show) $ forest)
             in (serverConsole %~ alterToKeyMap (appendConsoleMsg msg) drvId)

listener ::
  FilePath ->
  ServerSession ->
  TQueue (IO ()) ->
  IO ()
listener socketFile ssess workQ = do
  runServer socketFile $ \sock -> do
    _ <- forkIO $ sender sock
    receiver sock
  where
    ssRef = ssess ^. ssServerStateRef
    chanSignal = ssess ^. ssSubscriberSignal
    sender sock = forever $ do
      putStrLn $ "########"
      newPauseState <- atomically $ readTChan chanSignal
      putStrLn "#### sendObject ####"
      newPauseState `seq` sendObject sock newPauseState
    receiver sock = forever $ do
      msgs :: [ChanMessageBox] <- receiveObject sock
      F.for_ msgs $ \(CMBox o) -> do
        case o of
          CMSession s' -> do
            let mgi = sessionModuleGraph s'
            void $ forkIO (moduleGraphWorker ssRef mgi)
          CMHsSource _drvId (HsSourceInfo hiefile) ->
            void $ forkIO (hieWorker ssRef workQ hiefile)
          CMPaused drvId loc -> do
            mmodu <-
              atomically $ do
                ss <- readTVar ssRef
                let drvModMap = ss ^. serverDriverModuleMap
                pure $ forwardLookup drvId drvModMap
            case mmodu of
              Nothing -> do
                TIO.putStrLn $
                  "paused GHC at driverId = "
                    <> T.pack (show (unDriverId drvId))
                    <> ": "
                    <> T.pack (show loc)
              Just modu ->
                TIO.putStrLn $
                  "paused GHC at moduleName = "
                    <> modu
                    <> ": "
                    <> T.pack (show loc)
          _ -> pure ()
        atomically . modifyTVar' ssRef . updateInbox $ CMBox o
