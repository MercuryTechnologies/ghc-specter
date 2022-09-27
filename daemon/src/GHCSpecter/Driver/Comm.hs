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
  )
import Control.Lens ((%~), (.~), (^.))
import Control.Monad (forever, void)
import Data.Foldable qualified as F
import Data.Map.Strict qualified as M
import GHCSpecter.Channel
  ( ChanMessage (..),
    ChanMessageBox (..),
    Channel (..),
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
  ( HasServerState (..),
    ServerState (..),
    incrementSN,
  )
import GHCSpecter.Worker.Hie (hieWorker)
import GHCSpecter.Worker.ModuleGraph (moduleGraphWorker)

updateInbox :: ChanMessageBox -> ServerState -> ServerState
updateInbox chanMsg = incrementSN . updater
  where
    updater = case chanMsg of
      CMBox (CMCheckImports modu msg) ->
        (serverInbox %~ M.insert (CheckImports, modu) msg)
      CMBox (CMTiming modu timer') ->
        let f Nothing = Just timer'
            f (Just timer0) =
              Just $ Timer (unTimer timer0 ++ unTimer timer')
         in (serverTiming %~ M.alter f modu)
      CMBox (CMSession s') ->
        (serverSessionInfo .~ s')
      CMBox (CMHsSource _modu _info) ->
        id

listener ::
  FilePath ->
  TVar ServerState ->
  ServerSession ->
  TQueue (IO ()) ->
  IO ()
listener socketFile ssRef ssess workQ = do
  runServer socketFile $ \sock -> do
    _ <- forkIO $ sender sock
    receiver sock
  where
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
          CMHsSource _modu (HsSourceInfo hiefile) ->
            void $ forkIO (hieWorker ssRef workQ hiefile)
          _ -> pure ()
        atomically . modifyTVar' ssRef . updateInbox $ CMBox o