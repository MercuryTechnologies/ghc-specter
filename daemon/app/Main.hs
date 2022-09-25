{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Concur.Core (Widget, liftSTM, unsafeBlockingIO)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, forkOS)
import Control.Concurrent.STM
  ( TChan,
    TQueue,
    TVar,
    atomically,
    modifyTVar',
    newTChanIO,
    newTQueueIO,
    newTVar,
    newTVarIO,
    readTChan,
    readTQueue,
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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
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
import GHCSpecter.Config
  ( Config (..),
    defaultGhcSpecterConfigFile,
    loadConfig,
  )
import GHCSpecter.Data.Assets qualified as Assets
import GHCSpecter.Driver qualified as Driver
import GHCSpecter.Render (render)
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState (..),
    emptyServerState,
    incrementSN,
  )
import GHCSpecter.UI.ConcurReplica.Run (runDefaultWithStyle)
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
  ( BackgroundEvent (RefreshUI),
    Event (BkgEv),
  )
import GHCSpecter.Worker.Hie (hieWorker)
import GHCSpecter.Worker.ModuleGraph (moduleGraphWorker)
import Options.Applicative qualified as OA

data CLIMode
  = Online (Maybe FilePath)
  | View (Maybe FilePath)

onlineMode :: OA.Mod OA.CommandFields CLIMode
onlineMode =
  OA.command "online" $
    OA.info
      ( Online
          <$> OA.optional
            (OA.strOption (OA.long "config" <> OA.short 'c' <> OA.help "config file"))
      )
      (OA.progDesc "GHC inspection on the fly")

viewMode :: OA.Mod OA.CommandFields CLIMode
viewMode =
  OA.command "view" $
    OA.info
      ( View
          <$> OA.optional
            (OA.strOption (OA.long "config" <> OA.short 'c' <> OA.help "config file"))
      )
      (OA.progDesc "viewing saved session")

optsParser :: OA.ParserInfo CLIMode
optsParser =
  OA.info
    (OA.subparser (onlineMode <> viewMode) OA.<**> OA.helper)
    OA.fullDesc

runWorkQueue :: TQueue (IO ()) -> IO ()
runWorkQueue workQ = go 0
  where
    go :: Int -> IO ()
    go n = do
      job <- atomically $ readTQueue workQ
      -- TODO: disabled as it's too verbose. enable with proper logging system.
      -- putStrLn $ show n ++ "th job started"
      job
      -- putStrLn $ show n ++ "th job ended"
      go (n + 1)

listener :: FilePath -> TVar ServerState -> TQueue (IO ()) -> IO ()
listener socketFile ssRef workQ = do
  ss <- atomically $ readTVar ssRef
  runServer socketFile $ \sock -> do
    _ <- forkIO $ sender sock (ss ^. serverSessionInfo . to sessionIsPaused)
    receiver sock
  where
    sender sock lastState = do
      newState <-
        atomically $ do
          ss' <- readTVar ssRef
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
            void $ forkIO (moduleGraphWorker ssRef mgi)
          CMHsSource _modu (HsSourceInfo hiefile) ->
            void $ forkIO (hieWorker ssRef workQ hiefile)
          _ -> pure ()
        atomically . modifyTVar' ssRef . updateInbox $ CMBox o

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

-- | communication channel that UI renderer needs
-- Note that subscribe/publish is named according to UI side semantics.
data UIChannel = UIChannel
  { uiPublisherEvent :: TChan Event
  -- ^ channel for sending event to control
  , uiSubscriberState :: TChan (UIState, ServerState)
  -- ^ channel for receiving state from control
  , uiSubscriberBkgEvent :: TChan BackgroundEvent
  -- ^ channel for receiving background event
  }

styleText :: Text
styleText = "ul > li { margin-left: 10px; }"

webServer :: TVar ServerState -> IO ()
webServer ssRef = do
  assets <- Assets.loadAssets
  runDefaultWithStyle 8080 "ghc-specter" styleText $
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
      let newCS = Driver.ClientSession uiRef ssRef chanEv chanState chanBkg
          newUIChan = UIChannel chanEv chanState chanBkg
      unsafeBlockingIO $ Driver.main newCS
      loopM (step newUIChan) (BkgEv RefreshUI)
  where
    -- A single step of the outer loop (See Note [Control Loops]).
    step ::
      -- UI comm channel
      UIChannel ->
      -- last event
      Event ->
      Widget IHTML (Either Event ())
    step (UIChannel chanEv chanState chanBkg) ev = do
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

withConfig :: Maybe FilePath -> (Config -> IO ()) -> IO ()
withConfig mconfigFile action = do
  let config = fromMaybe defaultGhcSpecterConfigFile mconfigFile
  ecfg <- loadConfig config
  case ecfg of
    Left err -> putStrLn err
    Right cfg -> action cfg

main :: IO ()
main = do
  mode <- OA.execParser optsParser
  case mode of
    Online mconfigFile ->
      withConfig mconfigFile $ \cfg -> do
        let socketFile = configSocket cfg
        serverSessionRef <- atomically $ newTVar emptyServerState
        workQ :: TQueue (IO ()) <- newTQueueIO
        _ <- forkOS $ listener socketFile serverSessionRef workQ
        _ <- forkOS $ runWorkQueue workQ
        webServer serverSessionRef
    View mconfigFile ->
      withConfig mconfigFile $ \cfg -> do
        let sessionFile = configSessionFile cfg
        lbs <- BL.readFile sessionFile
        case eitherDecode' lbs of
          Left err -> print err
          Right ss -> do
            serverSessionRef <- atomically $ newTVar ss
            webServer serverSessionRef
