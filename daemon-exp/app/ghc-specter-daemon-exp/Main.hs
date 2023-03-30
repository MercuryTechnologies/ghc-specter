{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkOS, threadDelay)
import Control.Concurrent.STM (
  atomically,
  modifyTVar',
  newTChanIO,
  newTQueueIO,
  newTVar,
  newTVarIO,
  readTChan,
  readTVar,
  writeTChan,
 )
import Control.Lens (to, (.~), (^.))
import Control.Monad (forever)
import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (liftIO)
import Data.GI.Base (AttrOp ((:=)), new, on)
import Data.GI.Gtk.Threading (postGUIASync)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.Traversable (for)
import GHCSpecter.Channel.Outbound.Types (ModuleGraphInfo (..))
import GHCSpecter.Config (
  Config (..),
  defaultGhcSpecterConfigFile,
  loadConfig,
 )
import GHCSpecter.Control.Types (
  Control,
  nextEvent,
  printMsg,
 )
import GHCSpecter.Driver.Comm qualified as Comm
import GHCSpecter.Driver.Session qualified as Session (main)
import GHCSpecter.Driver.Session.Types (
  ClientSession (..),
  ServerSession (..),
  UIChannel (..),
 )
import GHCSpecter.Server.Types (
  HasModuleGraphState (..),
  HasServerState (..),
  HasTimingState (..),
  ServerState (..),
  initServerState,
 )
import GHCSpecter.UI.Types (
  HasUIState (..),
  UIView (..),
  emptyMainView,
  emptyUIState,
 )
import GHCSpecter.UI.Types.Event (
  BackgroundEvent (RefreshUI),
  Event (BkgEv),
 )
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector qualified as RC
import GI.Gtk qualified as Gtk
import GI.PangoCairo qualified as PC
import ModuleGraph (renderModuleGraph)
import Types (Tab (..), ViewBackend (..), ViewModel (..))
import Util (drawText)

withConfig :: Maybe FilePath -> (Config -> IO ()) -> IO ()
withConfig mconfigFile action = do
  let config = fromMaybe defaultGhcSpecterConfigFile mconfigFile
  ecfg <- loadConfig config
  case ecfg of
    Left err -> putStrLn err
    Right cfg -> do
      print cfg
      action cfg

initViewBackend :: IO (Maybe ViewBackend)
initViewBackend = do
  fontMap :: PC.FontMap <- PC.fontMapGetDefault
  pangoCtxt <- #createContext fontMap
  family <- #getFamily fontMap "FreeSans"
  mface <- #getFace family Nothing
  for mface $ \face -> do
    desc <- #describe face
    pure (ViewBackend pangoCtxt desc)

renderNotConnected :: ViewBackend -> R.Render ()
renderNotConnected vb = do
  R.setSourceRGBA 0 0 0 1
  drawText vb 36 (100, 100) "GHC is not connected yet"

renderAction ::
  ViewBackend ->
  ViewModel ->
  ServerState ->
  R.Render ()
renderAction vb vm ss = do
  let nameMap =
        ss ^. serverModuleGraphState . mgsModuleGraphInfo . to mginfoModuleNameMap
      drvModMap = ss ^. serverDriverModuleMap
      timing = ss ^. serverTiming . tsTimingMap
      mgs = ss ^. serverModuleGraphState
      clustering = mgs ^. mgsClustering
      mgrvis = mgs ^. mgsClusterGraph
  case mgrvis of
    Nothing -> renderNotConnected vb
    Just grVisInfo ->
      renderModuleGraph vb nameMap drvModMap timing clustering grVisInfo

forceUpdateLoop :: Gtk.DrawingArea -> IO ()
forceUpdateLoop drawingArea = forever $ do
  threadDelay 1_000_000
  postGUIASync $
    #queueDraw drawingArea

controlMain :: Control ()
controlMain = forever $ do
  printMsg "control tick"
  _ <- nextEvent
  pure ()

simpleEventLoop :: UIChannel -> IO ()
simpleEventLoop (UIChannel chanEv chanState chanBkg) = loopM step (BkgEv RefreshUI)
  where
    step ev = do
      atomically $ writeTChan chanEv ev
      (_ui, _ss) <- atomically $ readTChan chanState
      bev' <- atomically $ readTChan chanBkg
      pure (Left (BkgEv bev'))

main :: IO ()
main =
  withConfig Nothing $ \cfg -> do
    -- server session
    let socketFile = configSocket cfg
        nodeSizeLimit = configModuleClusterSize cfg
    ssRef <- atomically $ newTVar (initServerState nodeSizeLimit)
    chanSignal <- newTChanIO
    let servSess = ServerSession ssRef chanSignal
    -- client session
    -- TODO: Until necessary, we just put undefined assets. Later, change this properly.
    let assets = undefined
    initTime <- getCurrentTime
    let ui0 = emptyUIState assets initTime
        ui0' = (uiView .~ MainMode emptyMainView) ui0
    uiRef <- newTVarIO ui0'
    chanEv <- newTChanIO
    chanState <- newTChanIO
    chanBkg <- newTChanIO
    let
      -- client session
      cliSess = ClientSession uiRef chanEv chanState chanBkg
      -- UIChannel
      uiChan = UIChannel chanEv chanState chanBkg

    workQ <- newTQueueIO
    vmRef <- atomically $ newTVar (ViewModel TabModuleGraph)

    _ <- Gtk.init Nothing
    mvb <- initViewBackend
    case mvb of
      Nothing -> error "cannot initialize pango"
      Just vb0 -> do
        vbRef <- atomically $ newTVar vb0
        mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
        _ <- mainWindow `on` #destroy $ Gtk.mainQuit
        drawingArea <- new Gtk.DrawingArea []
        _ <- drawingArea
          `on` #draw
          $ RC.renderWithContext
          $ do
            (ss, vb, vm) <- liftIO $ atomically ((,,) <$> readTVar ssRef <*> readTVar vbRef <*> readTVar vmRef)
            renderAction vb vm ss
            pure True
        layout <- do
          vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]
          #packStart vbox drawingArea True True 0
          pure vbox
        #add mainWindow layout
        #setDefaultSize mainWindow 1440 768
        #showAll mainWindow
        _ <- forkOS $ Comm.listener socketFile servSess workQ
        _ <- forkOS $ Session.main servSess cliSess controlMain
        _ <- forkOS $ simpleEventLoop uiChan
        _ <- forkOS (forceUpdateLoop drawingArea)
        Gtk.main
