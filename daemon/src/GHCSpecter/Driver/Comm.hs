{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Control.Monad (forever, void)
import Data.Foldable qualified as F
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
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
import GHCSpecter.Data.Map
  ( alterToKeyMap,
    forwardLookup,
    insertToBiKeyMap,
  )
import GHCSpecter.Driver.Session.Types
  ( ServerSession (..),
  )
import GHCSpecter.Server.Types
  ( ConsoleItem (..),
    ModuleGraphState (..),
    ServerState (..),
    SupplementaryView (..),
    TimingState (..),
    incrementSN,
  )
import GHCSpecter.Worker.Hie
  ( hieWorker,
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
        \ss -> ss {_serverInbox = M.insert (CheckImports, modu) msg ss._serverInbox}
      CMBox (CMModuleInfo drvId modu mfile) ->
        let msg = ConsoleText ("module name: " <> modu <> ", file: " <> T.pack (show mfile))
         in \ss ->
              ss
                { _serverDriverModuleMap = insertToBiKeyMap (drvId, modu) ss._serverDriverModuleMap,
                  _serverConsole = alterToKeyMap (appendConsoleMsg msg) drvId ss._serverConsole
                }
      CMBox (CMTiming drvId timer') ->
        let f Nothing = Just timer'
            f (Just timer0) =
              Just $ Timer (unTimer timer0 ++ unTimer timer')
         in \ss ->
              let timing_map = ss._serverTiming._tsTimingMap
                  timing_map' = alterToKeyMap f drvId timing_map
               in ss
                    { _serverTiming =
                        ss._serverTiming
                          { _tsTimingMap = timing_map'
                          }
                    }
      CMBox (CMSession s') ->
        (\ss -> ss {_serverSessionInfo = s'})
      CMBox (CMModuleGraph mgi _msrcs) ->
        ( \ss ->
            ss
              { _serverModuleGraphState =
                  ss._serverModuleGraphState
                    { _mgsModuleGraphInfo = mgi
                    }
              }
        )
      CMBox (CMHsHie _ _) ->
        id
      CMBox (CMPaused drvId loc) ->
        let msg = ConsoleText ("paused at " <> T.pack (show loc))
            updateSessionInfo sinfo = sinfo {sessionIsPaused = True}
         in \ss ->
              ss
                { _serverSessionInfo = updateSessionInfo ss._serverSessionInfo,
                  _serverPaused = alterToKeyMap (const (Just loc)) drvId ss._serverPaused,
                  _serverConsole = alterToKeyMap (appendConsoleMsg msg) drvId ss._serverConsole
                }
      CMBox (CMConsole drvId creply) ->
        case creply of
          ConsoleReplyText mtab txt ->
            let msg = ConsoleText txt
                upd1 ss = ss {_serverConsole = alterToKeyMap (appendConsoleMsg msg) drvId ss._serverConsole}
                upd2 ss = fromMaybe ss $ do
                  tab <- mtab
                  modu <- forwardLookup drvId (ss._serverDriverModuleMap)
                  let item = SuppViewText txt
                  let append Nothing = Just [((tab, 0), item)]
                      append (Just xs) =
                        let n = length $ filter (\((tab', _), _) -> tab' == tab) xs
                         in Just (xs ++ [((tab, n), item)])
                      ss' =
                        ss {_serverSuppView = M.alter append modu ss._serverSuppView}
                  pure ss'
             in upd2 . upd1
          ConsoleReplyCoreBindList bindList ->
            let mkButton n = (n, ":print-core " <> n)
                msg = ConsoleButton (fmap (fmap mkButton) bindList)
             in \ss ->
                  ss {_serverConsole = alterToKeyMap (appendConsoleMsg msg) drvId ss._serverConsole}
          ConsoleReplyCore forest ->
            let msg = ConsoleCore forest
             in \ss ->
                  ss {_serverConsole = alterToKeyMap (appendConsoleMsg msg) drvId ss._serverConsole}

-- TODO: These all should be part of Control, not here.
invokeWorker :: TVar ServerState -> TQueue (IO ()) -> ChanMessageBox -> IO ()
invokeWorker ssRef workQ (CMBox o) =
  case o of
    CMCheckImports {} -> pure ()
    CMModuleInfo {} -> pure ()
    CMTiming {} -> pure ()
    CMSession {} -> pure ()
    CMModuleGraph mgi modSrcs -> do
      void $ forkIO (moduleSourceWorker ssRef modSrcs)
      void $ forkIO (moduleGraphWorker ssRef mgi)
    CMHsHie _drvId hiefile ->
      void $ forkIO (hieWorker ssRef workQ hiefile)
    CMPaused drvId loc -> do
      mmodu <-
        atomically $ do
          ss <- readTVar ssRef
          let drvModMap = ss._serverDriverModuleMap
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
    ssRef = ssess._ssServerStateRef
    chanSignal = ssess._ssSubscriberSignal
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
