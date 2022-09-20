{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Concur.Core (Widget, liftSTM, unsafeBlockingIO)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, forkOS)
import Control.Concurrent.STM
  ( TChan,
    TVar,
    atomically,
    modifyTVar',
    newTChanIO,
    newTVar,
    newTVarIO,
    readTChan,
    readTVar,
    retry,
    writeTChan,
  )
import Control.Lens (to, (%~), (.~), (^.))
import Control.Monad (forever, void)
import Control.Monad.Extra (loopM)
import Data.Aeson (eitherDecode')
import Data.ByteString.Lazy qualified as BL
import Data.Foldable qualified as F
import Data.Map.Strict qualified as M
import Data.Time.Clock (getCurrentTime)
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
import GHCSpecter.Driver (driver)
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
import GHCSpecter.UI.Types
  ( HasUIState (..),
    UIState,
    UIView (..),
    emptyMainView,
    emptyUIState,
  )
import GHCSpecter.UI.Types.Event
  ( BackgroundEvent (UITick),
    Event (BkgEv),
  )
import GHCSpecter.Worker.Hie (hieWorker)
import GHCSpecter.Worker.ModuleGraph (moduleGraphWorker)
import Options.Applicative qualified as OA

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

webServer :: TVar ServerState -> IO ()
webServer ssRef = do
  assets <- Assets.loadAssets
  runDefault 8080 "ghc-specter" $
    \_ -> do
      uiRef <-
        unsafeBlockingIO $ do
          initTime <- getCurrentTime
          let ui0 = emptyUIState assets initTime
              ui0' = (uiView .~ MainMode emptyMainView) ui0
          newTVarIO ui0'
      chanEv <- unsafeBlockingIO newTChanIO
      chanState <- unsafeBlockingIO newTChanIO
      chanBkg <- unsafeBlockingIO newTChanIO
      unsafeBlockingIO $ driver (uiRef, ssRef) chanEv chanState chanBkg
      loopM (step chanEv chanState chanBkg) (BkgEv UITick)
  where
    -- A single step of the outer loop (See Note [Control Loops]).
    step ::
      -- channel for sending event to control
      TChan Event ->
      -- channel for receiving state from control
      TChan (UIState, ServerState) ->
      -- channel for receiving background event
      TChan BackgroundEvent ->
      -- last event
      Event ->
      Widget IHTML (Either Event ())
    step chanEv chanState chanBkg ev = do
      (ui, ss) <-
        unsafeBlockingIO $ do
          atomically $ writeTChan chanEv ev
          (ui, ss) <- atomically $ readTChan chanState
          pure (ui, ss)
      stepRender (ui, ss) <|> (Left . BkgEv <$> waitForBkgEv chanBkg)

    stepRender :: (UIState, ServerState) -> Widget IHTML (Either Event ())
    stepRender (ui, ss) = do
      let renderUI =
            if ui ^. uiShouldUpdate
              then unblockDOMUpdate (render (ui, ss))
              else blockDOMUpdate (render (ui, ss))
      Left <$> renderUI

    waitForBkgEv ::
      -- channel for receiving bkg event
      TChan BackgroundEvent ->
      Widget IHTML BackgroundEvent
    waitForBkgEv chanBkg = liftSTM $ readTChan chanBkg

main :: IO ()
main = do
  mode <- OA.execParser optsParser
  case mode of
    Online socketFile -> do
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
