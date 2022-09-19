{-# LANGUAGE GADTs #-}

module Main (main) where

import Concur.Core (liftSTM, unsafeBlockingIO)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, forkOS, threadDelay)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar',
    newTVar,
    newTVarIO,
    readTVar,
    retry,
  )
import Control.Lens (to, (%~), (.~), (^.))
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
  ( blockDOMUpdate,
    unblockDOMUpdate,
  )
import GHCSpecter.UI.Constants
  ( chanUpdateInterval,
    tickInterval,
    uiUpdateInterval,
  )
import GHCSpecter.UI.Types
  ( HasUIState (..),
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

data ClientSession = ClientSession
  { csFrame :: Int
  , csFrameTime :: UTCTime
  }

driver :: TVar ServerState -> TVar ClientSession -> IO ()
driver serverSessionRef clientSessionRef = do
  initClientSession <-
    atomically $ readTVar clientSessionRef
  -- start mainConnector
  _ <- forkIO $ uiDriver initClientSession
  -- start chanConnector
  lastMessageSN <-
    (^. serverMessageSN) <$> atomically (readTVar serverSessionRef)
  _ <- forkIO $ chanDriver lastMessageSN
  pure ()
  where
    pollClientSession lastFrame = do
      ClientSession newFrame newFrameTime <- readTVar clientSessionRef
      if newFrame == lastFrame
        then retry
        else pure (ClientSession newFrame newFrameTime)

    blockUntilNewMessage lastSN = do
      ss <- readTVar serverSessionRef
      if (ss ^. serverMessageSN == lastSN)
        then retry
        else pure (ss ^. serverMessageSN)

    -- connector between driver and UI frame
    uiDriver (ClientSession lastFrame lastFrameTime) = do
      ClientSession newFrame newFrameTime <-
        atomically $ pollClientSession lastFrame
      putStrLn $ "client frame = " <> show newFrame <> " at " <> show newFrameTime
      uiDriver (ClientSession newFrame newFrameTime)

    -- background connector between server channel and UI frame
    chanDriver lastMessageSN = do
      -- wait for next poll
      threadDelay (floor (nominalDiffTimeToSeconds chanUpdateInterval * 1_000_000))
      -- blocked until a new message comes
      newMessageSN <-
        atomically $
          blockUntilNewMessage lastMessageSN
      putStrLn "MessageChanUpdate will be fired here"
      chanDriver newMessageSN

webServer :: TVar ServerState -> IO ()
webServer serverSessionRef = do
  assets <- Assets.loadAssets
  ss0 <- atomically (readTVar serverSessionRef)
  initTime <- getCurrentTime
  runDefault 8080 "ghc-specter" $
    \_ -> do
      clientSessionRef <-
        unsafeBlockingIO $ do
          zeroFrameTime <- getCurrentTime
          clientSessionRef <- newTVarIO (ClientSession 0 zeroFrameTime)
          driver serverSessionRef clientSessionRef
          pure clientSessionRef
      runStateT
        (loopM (step clientSessionRef) (UITick, \_ -> Control.main))
        (emptyUIState assets initTime, ss0)
  where
    -- A single step of the outer loop (See Note [Control Loops]).
    step ::
      TVar ClientSession ->
      (Event, Event -> Control ()) ->
      Runner (Either (Event, Event -> Control ()) ())
    step clientSessionRef (ev, c) = do
      lift $
        unsafeBlockingIO $ do
          now <- getCurrentTime
          atomically $
            modifyTVar'
              clientSessionRef
              (\(ClientSession n _) -> ClientSession (n + 1) now)
      result <- stepControlUpToEvent ev c
      ev' <- stepRender
      case result of
        Left c' -> pure (Left (ev', c'))
        Right r -> pure (Right r)

    stepRender :: Runner Event
    stepRender = do
      (ui, ss) <- get
      stepStartTime <- lift $ unsafeBlockingIO getCurrentTime

      let -- lastUpdatedServer = ss ^. serverLastUpdated
          tick :: Runner Event
          tick = do
            liftIO $ threadDelay (floor (nominalDiffTimeToSeconds tickInterval * 1_000_000))
            pure UITick
      {-
                await preMessageTime = do
                  when (preMessageTime `diffUTCTime` lastUpdatedServer < chanUpdateInterval) $
                    -- note: liftIO yields.
                    liftIO $
                      threadDelay (floor (nominalDiffTimeToSeconds chanUpdateInterval * 1_000_000))
                  -- lock until new message comes
                  ss' <- lift $
                    liftSTM $ do
                      ss' <- readTVar serverSessionRef
                      if (ss ^. serverMessageSN == ss' ^. serverMessageSN)
                        then retry
                        else pure ss'
                  postMessageTime <- lift $ unsafeBlockingIO getCurrentTime
                  let ss'' = (serverLastUpdated .~ postMessageTime) ss'
                  put (ui, ss'')
                  pure MessageChanUpdated
      -}
      let renderUI =
            if ui ^. uiShouldUpdate
              then lift (unblockDOMUpdate (render (ui, ss)))
              else lift (blockDOMUpdate (render (ui, ss)))
      renderUI <|> {- await stepStartTime <|> -} tick
