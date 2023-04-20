{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config (appWidgetConfig)
import Control.Concurrent (forkOS)
import Control.Concurrent.STM (
  atomically,
  flushTQueue,
  newTChanIO,
  newTQueueIO,
  newTVar,
  newTVarIO,
  readTChan,
  readTVar,
  retry,
  takeTMVar,
  writeTChan,
  writeTQueue,
  writeTVar,
 )
import Control.Lens (at, to, (&), (.~), (^.), _1, _2)
import Control.Monad (join)
import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable (traverse_)
import Data.GI.Base (AttrOp ((:=)), new, on)
import Data.GI.Gtk.Threading (postGUIASync)
import Data.IORef (newIORef)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Traversable (for)
import Data.Typeable (gcast)
import GHCSpecter.Channel.Outbound.Types (ModuleGraphInfo (..))
import GHCSpecter.Config (
  Config (..),
  defaultGhcSpecterConfigFile,
  loadConfig,
 )
import GHCSpecter.Control qualified as Control
import GHCSpecter.Control.Runner (RunnerEnv (..))
import GHCSpecter.Driver.Comm qualified as Comm
import GHCSpecter.Driver.Session qualified as Session (main)
import GHCSpecter.Driver.Session.Types (
  ClientSession (..),
  HasClientSession (..),
  HasServerSession (..),
  ServerSession (..),
  UIChannel (..),
 )
import GHCSpecter.Driver.Worker qualified as Worker
import GHCSpecter.Graphics.DSL (
  Color (Black),
  EventMap,
  TextFontFace (Sans),
  ViewPort (..),
 )
import GHCSpecter.Server.Types (
  HasModuleGraphState (..),
  HasServerState (..),
  HasTimingState (..),
  ServerState (..),
  initServerState,
 )
import GHCSpecter.UI.Constants (
  canvasDim,
  modGraphHeight,
  modGraphWidth,
 )
import GHCSpecter.UI.Types (
  HasModuleGraphUI (..),
  HasSessionUI (..),
  HasSourceViewUI (..),
  HasTimingUI (..),
  HasUIModel (..),
  HasUIState (..),
  HasWidgetConfig (..),
  UIState,
  ViewPortInfo (..),
  emptyUIState,
 )
import GHCSpecter.UI.Types.Event (
  BackgroundEvent (RefreshUI),
  DetailLevel (..),
  Event (..),
  Tab (..),
 )
import GHCSpecter.Util.Transformation (translateToOrigin)
import GHCSpecter.Util.Transformation qualified as Transformation (hitScene)
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector qualified as RC
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk
import GI.PangoCairo qualified as PC
import Handler (
  handleClick,
  handleMotion,
  handleScroll,
  handleZoomEnd,
  handleZoomUpdate,
 )
import Render.ModuleGraph (renderModuleGraph)
import Render.Session (renderSession)
import Render.SourceView (renderSourceView)
import Render.Timing (renderTiming)
import Renderer (drawText, setColor)
import Types (
  GtkRender,
  ViewBackend (..),
  ViewBackendResource (..),
  WrappedViewBackend (..),
 )

detailLevel :: DetailLevel
detailLevel = UpTo30

withConfig :: Maybe FilePath -> (Config -> IO ()) -> IO ()
withConfig mconfigFile action = do
  let config = fromMaybe defaultGhcSpecterConfigFile mconfigFile
  ecfg <- loadConfig config
  case ecfg of
    Left err -> putStrLn err
    Right cfg -> do
      print cfg
      action cfg

initViewBackendResource :: IO (Maybe ViewBackendResource)
initViewBackendResource = do
  fontMap :: PC.FontMap <- PC.fontMapGetDefault
  pangoCtxt <- #createContext fontMap
  familySans <- #getFamily fontMap "FreeSans"
  mfaceSans <- #getFace familySans Nothing
  familyMono <- #getFamily fontMap "FreeMono"
  mfaceMono <- #getFace familyMono Nothing
  for ((,) <$> mfaceSans <*> mfaceMono) $ \(faceSans, faceMono) -> do
    descSans <- #describe faceSans
    descMono <- #describe faceMono
    pure $
      ViewBackendResource
        { vbrPangoContext = pangoCtxt
        , vbrFontDescSans = descSans
        , vbrFontDescMono = descMono
        , vbrWidgetConfig = appWidgetConfig
        }

renderNotConnected :: GtkRender e ()
renderNotConnected = do
  lift R.save
  setColor Black
  drawText Sans 36 (100, 100) "GHC is not connected yet"
  lift R.restore

renderAction ::
  UIState ->
  ServerState ->
  GtkRender Text ()
renderAction ui ss = do
  let nameMap =
        ss ^. serverModuleGraphState . mgsModuleGraphInfo . to mginfoModuleNameMap
      drvModMap = ss ^. serverDriverModuleMap
      timing = ss ^. serverTiming . tsTimingMap
      mgs = ss ^. serverModuleGraphState
      clustering = mgs ^. mgsClustering
      mgrvis = mgs ^. mgsClusterGraph
      mgrui = ui ^. uiModel . modelMainModuleGraph
      sgrui = ui ^. uiModel . modelSubModuleGraph
      subgraphs = mgs ^. mgsSubgraph
      sessui = ui ^. uiModel . modelSession

  case mgrvis of
    Nothing -> renderNotConnected
    Just grVisInfo ->
      case ui ^. uiModel . modelTab of
        TabSession ->
          renderSession
            ss
            sessui
        TabModuleGraph ->
          renderModuleGraph
            (mgrui, sgrui)
            subgraphs
            nameMap
            drvModMap
            timing
            clustering
            grVisInfo
        TabSourceView -> do
          let srcUI = ui ^. uiModel . modelSourceView
          renderSourceView srcUI ss
        TabTiming -> do
          let tui = ui ^. uiModel . modelTiming
              ttable = ss ^. serverTiming . tsTimingTable
          renderTiming drvModMap tui ttable

simpleEventLoop :: UIChannel -> IO ()
simpleEventLoop (UIChannel chanEv chanState chanQEv) = loopM step (BkgEv RefreshUI)
  where
    step ev = do
      atomically $ writeTChan chanEv ev

      -- this is due to the compatibility with concur-replica.
      -- TODO: remove this properly.
      _ <- atomically $ readTChan chanState

      ev' <- atomically $ do
        evs' <- flushTQueue chanQEv
        -- Prioritize background events
        let (bevs', fevs') = partition (\case BkgEv _ -> True; _ -> False) evs'
        case bevs' of
          bev' : bevs'' -> do
            traverse_ (writeTQueue chanQEv) (bevs'' ++ fevs')
            pure bev'
          _ -> case fevs' of
            fev' : fevs'' -> do
              traverse_ (writeTQueue chanQEv) fevs''
              pure fev'
            _ -> retry
      pure (Left ev')

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
    let defVP = ViewPort (0, 0) (modGraphWidth, 0.5 * modGraphHeight)
        vpSessionMain =
          appWidgetConfig ^. wcfgSession . at "session-main" . to (maybe defVP translateToOrigin)
        vpMainModGraph =
          appWidgetConfig ^. wcfgModuleGraph . at "main-module-graph" . to (maybe defVP translateToOrigin)
        vpSubModGraph =
          appWidgetConfig ^. wcfgModuleGraph . at "sub-module-graph" . to (maybe defVP translateToOrigin)
        vpSrcModTree =
          appWidgetConfig ^. wcfgSourceView . at "module-tree" . to (maybe defVP translateToOrigin)
        vpSrcSource =
          appWidgetConfig ^. wcfgSourceView . at "source-view" . to (maybe defVP translateToOrigin)
        vpSrcSupp =
          appWidgetConfig ^. wcfgSourceView . at "supple-view-contents" . to (maybe defVP translateToOrigin)

    let ui0 = emptyUIState assets initTime
        ui0' =
          ui0
            & (uiModel . modelTab .~ TabModuleGraph)
              . ( uiModel . modelSession . sessionUIMainViewPort
                    .~ ViewPortInfo vpSessionMain Nothing
                )
              . ( uiModel . modelMainModuleGraph . modGraphViewPort
                    .~ ViewPortInfo vpMainModGraph Nothing
                )
              . ( uiModel . modelSubModuleGraph . _2 . modGraphViewPort
                    .~ ViewPortInfo vpSubModGraph Nothing
                )
              . (uiModel . modelWidgetConfig .~ appWidgetConfig)
              . ( uiModel . modelSourceView . srcViewModuleTreeViewPort
                    .~ ViewPortInfo vpSrcModTree Nothing
                )
              . ( uiModel . modelSourceView . srcViewSourceViewPort
                    .~ ViewPortInfo vpSrcSource Nothing
                )
              . ( uiModel . modelSourceView . srcViewSuppViewPort
                    .~ ViewPortInfo vpSrcSupp Nothing
                )
              . (uiModel . modelTiming . timingUIPartition .~ True)
              . (uiModel . modelTiming . timingUIHowParallel .~ False)

    uiRef <- newTVarIO ui0'
    chanEv <- newTChanIO
    chanState <- newTChanIO
    chanQEv <- newTQueueIO
    let
      -- client session
      cliSess = ClientSession uiRef chanEv chanState chanQEv
      -- UIChannel
      uiChan = UIChannel chanEv chanState chanQEv

    workQ <- newTQueueIO

    _ <- Gtk.init Nothing
    mvb <- initViewBackendResource
    case mvb of
      Nothing -> error "cannot initialize pango"
      Just vbr -> do
        vbRef <- atomically $ do
          emapRef <- newTVar ([] :: [EventMap Text])
          let vb = ViewBackend vbr emapRef
          newTVar (WrappedViewBackend vb)
        mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
        _ <- mainWindow `on` #destroy $ Gtk.mainQuit
        -- main canvas
        drawingArea <- new Gtk.DrawingArea []
        #addEvents
          drawingArea
          [ Gdk.EventMaskButtonPressMask
          , Gdk.EventMaskButtonReleaseMask
          , Gdk.EventMaskPointerMotionMask
          , Gdk.EventMaskScrollMask
          , Gdk.EventMaskTouchpadGestureMask
          ]

        _ <- drawingArea
          `on` #draw
          $ RC.renderWithContext
          $ do
            (vb, ui, ss) <- liftIO $ atomically $ do
              ui <- readTVar uiRef
              ss <- readTVar ssRef
              emapRef <- newTVar []
              let vb = ViewBackend vbr emapRef
              writeTVar vbRef (WrappedViewBackend vb)
              pure (vb, ui, ss)
            runReaderT (renderAction ui ss) vb
            pure True

        let refreshAction = postGUIASync (#queueDraw drawingArea)
        _ <- drawingArea
          `on` #buttonPressEvent
          $ \ev -> do
            handleClick chanQEv ev
            pure True
        _ <- drawingArea
          `on` #motionNotifyEvent
          $ \ev -> do
            handleMotion chanQEv ev
            pure True
        _ <- drawingArea
          `on` #scrollEvent
          $ \ev -> do
            handleScroll chanQEv ev
            pure True
        gzoom <- Gtk.gestureZoomNew drawingArea
        _ <- gzoom
          `on` #scaleChanged
          $ \scale -> do
            (_, xcenter, ycenter) <- #getBoundingBoxCenter gzoom
            handleZoomUpdate chanQEv (xcenter, ycenter) scale
        _ <- gzoom
          `on` #end
          $ \_ -> do
            handleZoomEnd chanQEv
        #setPropagationPhase gzoom Gtk.PropagationPhaseBubble

        layout <- do
          vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]
          #packStart vbox drawingArea True True 0
          pure vbox
        #add mainWindow layout
        #setDefaultSize mainWindow (canvasDim ^. _1) (canvasDim ^. _2)
        #showAll mainWindow

        -- prepare runner
        -- TODO: make common initialization function (but backend-dep)
        counterRef <- newIORef 0
        let runner =
              RunnerEnv
                { runnerCounter = counterRef
                , runnerUIState = cliSess ^. csUIStateRef
                , runnerServerState = servSess ^. ssServerStateRef
                , runnerQEvent = cliSess ^. csPublisherEvent
                , runnerSignalChan = servSess ^. ssSubscriberSignal
                , runnerRefreshAction = refreshAction
                , runnerHitScene = \xy -> atomically $ do
                    WrappedViewBackend vb <- readTVar vbRef
                    let emapRef = vbEventMap vb
                    emaps <- readTVar emapRef
                    let memap = Transformation.hitScene xy emaps
                    pure (join (gcast @_ @Text <$> memap))
                }
        _ <- forkOS $ Comm.listener socketFile servSess workQ
        _ <- forkOS $ Worker.runWorkQueue workQ
        _ <-
          forkOS $
            Session.main
              runner
              servSess
              cliSess
              Control.mainLoop
        _ <- forkOS $ simpleEventLoop uiChan
        Gtk.main
