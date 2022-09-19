{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Concur.Core (Widget, liftSTM, unsafeBlockingIO)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, forkOS, threadDelay)
import Control.Concurrent.STM
  ( STM,
    TMVar,
    TVar,
    atomically,
    check,
    modifyTVar',
    newEmptyTMVarIO,
    newTVar,
    newTVarIO,
    putTMVar,
    readTVar,
    retry,
    takeTMVar,
    writeTVar,
  )
import Control.Lens (makeClassy, to, (%~), (.~), (^.))
import Control.Monad (forever, void, when)
import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.State
  ( StateT (..),
    get,
    put,
    runStateT,
  )
import Data.Aeson (eitherDecode')
import Data.ByteString.Lazy qualified as BL
import Data.Foldable qualified as F
import Data.Map.Strict qualified as M
import Data.Time.Clock
  ( UTCTime,
    diffUTCTime,
    getCurrentTime,
    nominalDiffTimeToSeconds,
  )
import Debug.Trace (trace)
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
import GHCSpecter.Control qualified as Control (main)
import GHCSpecter.Control.Runner
  ( Runner,
    stepControlUpToEvent,
  )
import GHCSpecter.Control.Types (Control)
import GHCSpecter.Render (render)
import GHCSpecter.Render.Data.Assets qualified as Assets
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState (..),
    emptyServerState,
    incrementSN,
  )
import GHCSpecter.UI.ConcurReplica.Run (runDefault)
import GHCSpecter.UI.ConcurReplica.Types
  ( IHTML,
    blockDOMUpdate,
    unblockDOMUpdate,
  )
import GHCSpecter.UI.Constants
  ( chanUpdateInterval,
    tickInterval,
    uiUpdateInterval,
  )
import GHCSpecter.UI.Types
  ( HasMainView (..),
    HasUIState (..),
    UIState,
    UIView (..),
    emptyMainView,
    emptyUIState,
  )
import GHCSpecter.UI.Types.Event (Event (MessageChanUpdated, UITick))
import GHCSpecter.Worker.Hie (hieWorker)
import GHCSpecter.Worker.ModuleGraph (moduleGraphWorker)
import Options.Applicative qualified as OA
import Prelude hiding (div)

data CLIMode
  = Online FilePath
  | View FilePath

onlineMode :: OA.Mod OA.CommandFields CLIMode
onlineMode =
  OA.command "online" $
    OA.info
      (Online <$> OA.strOption (OA.long "socket-file" <> OA.short 's' <> OA.help "socket file"))
      (OA.progDesc "GHC inspection on the fly")

viewMode :: OA.Mod OA.CommandFields CLIMode
viewMode =
  OA.command "view" $
    OA.info
      (View <$> OA.strOption (OA.long "session-file" <> OA.short 'f' <> OA.help "session file"))
      (OA.progDesc "viewing saved session")

optsParser :: OA.ParserInfo CLIMode
optsParser =
  OA.info
    (OA.subparser (onlineMode <> viewMode) OA.<**> OA.helper)
    OA.fullDesc

listener :: FilePath -> TVar ServerState -> IO ()
listener socketFile var = do
  ss <- atomically $ readTVar var
  runServer socketFile $ \sock -> do
    _ <- forkIO $ sender sock (ss ^. serverSessionInfo . to sessionIsPaused)
    receiver sock
  where
    sender sock lastState = do
      newState <-
        atomically $ do
          ss' <- readTVar var
          let newState = ss' ^. serverSessionInfo . to sessionIsPaused
          if newState == lastState
            then retry
            else pure newState
      sendObject sock newState
      sender sock newState
    receiver sock = forever $ do
      msgs :: [ChanMessageBox] <- receiveObject sock
      F.for_ msgs $ \(CMBox o) -> do
        case o of
          CMSession s' -> do
            let mgi = sessionModuleGraph s'
            void $ forkIO (moduleGraphWorker var mgi)
          CMHsSource _modu (HsSourceInfo hiefile) -> do
            void $ forkIO (hieWorker var hiefile)
          _ -> pure ()
        atomically . modifyTVar' var . updateInbox $ CMBox o

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

-- NOTE:
-- server state: shared across the session
-- ui state: per web view.
-- control: per web view

driver ::
  (TVar UIState, TVar ServerState) ->
  TMVar Event ->
  IO ()
driver (uiRef, ssRef) lock = do
  -- start chanConnector
  -- lastMessageSN <-
  --   (^. serverMessageSN) <$> atomically (readTVar ssRef)
  -- _ <- forkIO $ chanDriver lastMessageSN
  _ <- forkIO $ controlDriver lock
  pure ()
  where
    blockUntilNewMessage lastSN = do
      ss <- readTVar ssRef
      if (ss ^. serverMessageSN == lastSN)
        then retry
        else pure (ss ^. serverMessageSN)

    -- background connector between server channel and UI frame
    chanDriver lastMessageSN = do
      -- wait for next poll
      threadDelay (floor (nominalDiffTimeToSeconds chanUpdateInterval * 1_000_000))
      -- blocked until a new message comes
      newMessageSN <-
        atomically $
          blockUntilNewMessage lastMessageSN
      {-
            atomically $ do
              cs <- readTVar clientSessionRef
              let newCF = cs ^. csFrame
              if newCF == lastCF
                then retry
                else do
                  let cs' = (csFrameState .~ Just (ev, now)) cs
                    writeTVar clientSessionRef cs'
                    pure (Left newCF)
      -}

      putStrLn "MessageChanUpdate will be fired here"
      chanDriver newMessageSN

    -- connector between driver and Control frame
    controlDriver lock = loopM step (\_ -> Control.main)
      where
        step c = do
          putStrLn "waiting for a lock"
          ev <- atomically $ takeTMVar lock
          putStrLn "got event"
          ec' <- runReaderT (stepControlUpToEvent ev c) (uiRef, ssRef)
          putStrLn "after process"
          -- atomically $
          --   modifyTVar' clientSessionRef ((csFrame %~ (+ 1)) . (csFrameState .~ Nothing))
          putStrLn "after CF update"
          pure ec'

webServer :: TVar ServerState -> IO ()
webServer ssRef = do
  assets <- Assets.loadAssets
  ss0 <- atomically (readTVar ssRef)
  runDefault 8080 "ghc-specter" $
    \_ -> do
      (uiRef, initTime) <-
        unsafeBlockingIO $ do
          initTime <- getCurrentTime
          let ui0 = emptyUIState assets initTime
              ui0' = (uiView .~ MainMode emptyMainView) ui0
          uiRef <- newTVarIO ui0'
          pure (uiRef, initTime)
      lock <- unsafeBlockingIO newEmptyTMVarIO
      unsafeBlockingIO $ driver (uiRef, ssRef) lock
      loopM (step (uiRef, ssRef) lock) UITick
  where
    -- A single step of the outer loop (See Note [Control Loops]).
    step ::
      (TVar UIState, TVar ServerState) ->
      TMVar Event ->
      Event ->
      Widget IHTML (Either Event ())
    step (uiRef, ssRef) lock ev = do
      (ui, ss) <-
        unsafeBlockingIO $ do
          putStrLn $ "step: " ++ show ev
          atomically $ putTMVar lock ev
          putStrLn $ "after putTMVar"
          (ui, ss) <-
            atomically $
              (,) <$> readTVar uiRef <*> readTVar ssRef
          case ui ^. uiView of
            BannerMode _ -> putStrLn "BannerMode"
            MainMode view -> do
              putStrLn $
                "MainMode: " ++ show (view ^. mainTab)
          pure (ui, ss)
      Left <$> stepRender (ui, ss)

    stepRender :: (UIState, ServerState) -> Widget IHTML Event
    stepRender (ui, ss) = do
      -- stepStartTime <- unsafeBlockingIO getCurrentTime
      {- let renderUI =
            if ui ^. uiShouldUpdate
              then unblockDOMUpdate (render (ui, ss))
              else bloc kDOMUpdate (render (ui, ss)) -}
      -- renderUI
      render (ui, ss)

main :: IO ()
main = do
  mode <- OA.execParser optsParser
  case mode of
    Online socketFile -> do
      now <- getCurrentTime
      serverSessionRef <- atomically $ newTVar emptyServerState
      _ <- forkOS $ listener socketFile serverSessionRef
      webServer serverSessionRef
    View sessionFile -> do
      lbs <- BL.readFile sessionFile
      case eitherDecode' lbs of
        Left err -> print err
        Right ss -> do
          serverSessionRef <- atomically $ newTVar ss
          webServer serverSessionRef
