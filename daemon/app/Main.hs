{-# LANGUAGE GADTs #-}

module Main (main) where

import Concur.Core (Widget, liftSTM, unsafeBlockingIO)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, forkOS, threadDelay)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar',
    newTVar,
    readTVar,
    retry,
    writeTVar,
  )
import Control.Lens (to, (%~), (.~), (^.))
import Control.Monad (forever, void, when)
import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode')
import Data.ByteString.Lazy qualified as BL
import Data.Foldable qualified as F
import Data.Map.Strict qualified as M
import Data.Time.Clock
  ( NominalDiffTime,
    diffUTCTime,
    getCurrentTime,
    nominalDiffTimeToSeconds,
    secondsToNominalDiffTime,
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
import GHCSpecter.Render (render)
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
import GHCSpecter.UI.Types
  ( HasUIState (..),
    UIState,
    emptyUIState,
  )
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
      var <- atomically $ newTVar (emptyServerState now)
      _ <- forkOS $ listener socketFile var
      webServer var
    View sessionFile -> do
      lbs <- BL.readFile sessionFile
      case eitherDecode' lbs of
        Left err -> print err
        Right ss -> do
          var <- atomically $ newTVar ss
          webServer var

chanUpdateInterval :: NominalDiffTime
chanUpdateInterval = secondsToNominalDiffTime (fromRational (1 / 2))

uiUpdateInterval :: NominalDiffTime
uiUpdateInterval = secondsToNominalDiffTime (fromRational (1 / 10))

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

webServer :: TVar ServerState -> IO ()
webServer var = do
  ss0 <- atomically (readTVar var)
  initTime <- getCurrentTime
  runDefault 8080 "test" $
    \_ -> loopM step (emptyUIState initTime, ss0)
  where
    step :: (UIState, ServerState) -> Widget IHTML (Either (UIState, ServerState) ())
    step (ui0, ss) = do
      let lastUpdatedUI = ui0 ^. uiLastUpdated
      stepStartTime <- unsafeBlockingIO getCurrentTime
      unsafeBlockingIO $ print stepStartTime
      let ui
            | (stepStartTime `diffUTCTime` lastUpdatedUI > uiUpdateInterval) = (uiShouldUpdate .~ True) ui0
            | otherwise = (uiShouldUpdate .~ False) ui0

      let lastUpdatedServer = ss ^. serverLastUpdated
          await preMessageTime = do
            when (preMessageTime `diffUTCTime` lastUpdatedServer < chanUpdateInterval) $
              -- note: liftIO yields.
              liftIO $
                threadDelay (floor (nominalDiffTimeToSeconds chanUpdateInterval * 1_000_000))
            -- lock until new message comes
            ss' <- liftSTM $ do
              ss' <- readTVar var
              if (ss ^. serverMessageSN == ss' ^. serverMessageSN)
                then retry
                else pure ss'
            postMessageTime <- unsafeBlockingIO getCurrentTime
            let ss'' = (serverLastUpdated .~ postMessageTime) ss'
            pure (ui, ss'')
          --
          updateSS (ui', (ss', b)) = do
            when b $
              liftSTM $ writeTVar var ss'
            pure (Left (ui', ss'))

      let renderUI0 = render stepStartTime (ui, ss) >>= updateSS
          -- wait for update interval, not to have too frequent update
          renderUI =
            if ui ^. uiShouldUpdate
              then do
                unsafeBlockingIO $ putStrLn "unblocking"
                unblockDOMUpdate renderUI0
              else do
                unsafeBlockingIO $ putStrLn "blocking"
                blockDOMUpdate renderUI0
      renderUI <|> (Left <$> await stepStartTime)
