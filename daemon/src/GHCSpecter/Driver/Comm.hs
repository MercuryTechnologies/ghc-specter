{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Driver.Comm (
  listener,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (
  TQueue,
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
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Channel.Outbound.Types (
  ChanMessage (..),
  ChanMessageBox (..),
  Channel (..),
  ConsoleReply (..),
  SessionInfo (..),
  Timer (..),
 )
import GHCSpecter.Comm (
  receiveObject,
  runServer,
  sendObject,
 )
import GHCSpecter.Data.Map (
  alterToKeyMap,
  forwardLookup,
  insertToBiKeyMap,
 )
import GHCSpecter.Driver.Session.Types (
  HasServerSession (..),
  ServerSession (..),
 )
import GHCSpecter.Server.Types (
  ConsoleItem (..),
  HasServerState (..),
  HasTimingState (..),
  ServerState (..),
  SupplementaryView (..),
  incrementSN,
 )
import GHCSpecter.Worker.Hie (
  hieWorker,
  moduleSourceWorker,
 )
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
         in (serverTiming . tsTimingMap %~ alterToKeyMap f drvId)
      CMBox (CMSession s') ->
        (serverSessionInfo .~ s')
      CMBox (CMHsHie _ _) ->
        id
      CMBox (CMPaused drvId loc) ->
        let msg = ConsoleText ("paused at " <> T.pack (show loc))
            updateSessionInfo sinfo = sinfo {sessionIsPaused = True}
         in (serverSessionInfo %~ updateSessionInfo)
              . (serverPaused %~ alterToKeyMap (const (Just loc)) drvId)
              . (serverConsole %~ alterToKeyMap (appendConsoleMsg msg) drvId)
      CMBox (CMConsole drvId creply) ->
        case creply of
          ConsoleReplyText mtab txt ->
            let msg = ConsoleText txt
                upd1 = (serverConsole %~ alterToKeyMap (appendConsoleMsg msg) drvId)
                upd2 ss = fromMaybe ss $ do
                  tab <- mtab
                  modu <- forwardLookup drvId (ss ^. serverDriverModuleMap)
                  let item = SuppViewText txt
                  let append Nothing = Just [((tab, 0), item)]
                      append (Just xs) =
                        let n = length $ filter (\((tab', _), _) -> tab' == tab) xs
                         in Just (xs ++ [((tab, n), item)])
                      ss' =
                        (serverSuppView %~ M.alter append modu) ss
                  pure ss'
             in upd2 . upd1
          ConsoleReplyCoreBindList bindList ->
            let mkButton n = (n, ":print-core " <> n)
                msg = ConsoleButton (fmap (fmap mkButton) bindList)
             in (serverConsole %~ alterToKeyMap (appendConsoleMsg msg) drvId)
          ConsoleReplyCore forest ->
            let msg = ConsoleCore forest
             in (serverConsole %~ alterToKeyMap (appendConsoleMsg msg) drvId)

-- TODO: These all should be part of Control, not here.
invokeWorker :: TVar ServerState -> TQueue (IO ()) -> ChanMessageBox -> IO ()
invokeWorker ssRef workQ (CMBox o) =
  case o of
    CMCheckImports {} -> pure ()
    CMModuleInfo {} -> pure ()
    CMTiming {} -> pure ()
    CMSession s' -> do
      let modSrcs = sessionModuleSources s'
          mgi = sessionModuleGraph s'
      void $ forkIO (moduleSourceWorker ssRef modSrcs)
      void $ forkIO (moduleGraphWorker ssRef mgi)
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
    CMConsole {} -> pure ()

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
        -- pure state update
        atomically . modifyTVar' ssRef . updateInbox $ msg

        -- async IO update
        invokeWorker ssRef workQ msg
