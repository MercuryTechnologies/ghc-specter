{-# LANGUAGE GADTs #-}

module GHCSpecter.Driver.Comm
  ( listener,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( TQueue,
    TVar,
    atomically,
    modifyTVar',
    readTChan,
    readTVar,
  )
import Control.Lens ((%~), (.~), (^.))
import Control.Monad (forever, void)
import Data.Foldable qualified as F
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Channel.Outbound.Types
  ( ChanMessage (..),
    ChanMessageBox (..),
    Channel (..),
    ConsoleReply (..),
    SessionInfo (..),
    Timer (..),
  )
import GHCSpecter.Comm
  ( receiveObject,
    runServer,
    sendObject,
  )
import GHCSpecter.Data.GHC.Hie
  ( HasModuleHieInfo (..),
    emptyModuleHieInfo,
  )
import GHCSpecter.Driver.Session.Types
  ( HasServerSession (..),
    ServerSession (..),
  )
import GHCSpecter.Server.Types
  ( ConsoleItem (..),
    HasHieState (..),
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
      CMBox (CMModuleInfo drvId modu mfile) ->
        let msg = ConsoleText ("module name: " <> modu <> ", file: " <> T.pack (show mfile))
         in (serverDriverModuleMap %~ insertToBiKeyMap (drvId, modu))
              . (serverConsole %~ alterToKeyMap (appendConsoleMsg msg) drvId)
      CMBox (CMTiming drvId timer') ->
        let f Nothing = Just timer'
            f (Just timer0) =
              Just $ Timer (unTimer timer0 ++ unTimer timer')
         in (serverTiming %~ alterToKeyMap f drvId)
      CMBox (CMSession s') ->
        (serverSessionInfo .~ s')
      CMBox (CMHsHie _ _) ->
        id
      CMBox (CMPaused drvId mloc) ->
        let formatMsg (Just loc) = ConsoleText ("paused at " <> T.pack (show loc))
            formatMsg Nothing = ConsoleText "resume"
            updateSessionInfo (Just _) = \sinfo -> sinfo {sessionIsPaused = True}
            updateSessionInfo Nothing = id
         in (serverSessionInfo %~ updateSessionInfo mloc)
              . (serverPaused %~ alterToKeyMap (const mloc) drvId)
              . (serverConsole %~ alterToKeyMap (appendConsoleMsg (formatMsg mloc)) drvId)
      CMBox (CMConsole drvId creply) ->
        case creply of
          ConsoleReplyText txt ->
            let msg = ConsoleText txt
             in (serverConsole %~ alterToKeyMap (appendConsoleMsg msg) drvId)
          ConsoleReplyCoreBindList bindList ->
            let mkButton n = (n, ":print-core " <> n)
                msg = ConsoleButton (fmap (fmap mkButton) bindList)
             in (serverConsole %~ alterToKeyMap (appendConsoleMsg msg) drvId)
          ConsoleReplyCore forest ->
            let msg = ConsoleCore forest
             in (serverConsole %~ alterToKeyMap (appendConsoleMsg msg) drvId)

invokeWorker :: TVar ServerState -> TQueue (IO ()) -> ChanMessageBox -> IO ()
invokeWorker ssRef workQ (CMBox o) =
  case o of
    CMSession s' -> do
      let mgi = sessionModuleGraph s'
      void $ forkIO (moduleGraphWorker ssRef mgi)
    CMModuleInfo _ modu mfile -> do
      src <-
        case mfile of
          Nothing -> pure ""
          Just file -> TIO.readFile file
      let modHie = (modHieSource .~ src) emptyModuleHieInfo
      atomically $
        modifyTVar' ssRef (serverHieState . hieModuleMap %~ M.insert modu modHie)
    CMHsHie _drvId hiefile ->
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
      F.for_ msgs $ \msg -> do
        invokeWorker ssRef workQ msg
        atomically . modifyTVar' ssRef . updateInbox $ msg
