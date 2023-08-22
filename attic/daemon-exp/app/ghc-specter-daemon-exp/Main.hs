{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkOS)
import Control.Concurrent.STM
  ( atomically,
    newTChanIO,
    newTQueueIO,
    newTVar,
    newTVarIO,
    readTVar,
    writeTVar,
  )
import Control.Lens (at, to, (&), (.~), (^.), _1, _2)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.GI.Base (AttrOp ((:=)), after, get, new, on)
import Data.GI.Gtk.Threading (postGUIASync)
import Data.IORef (newIORef)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.Traversable (for)
import Data.Typeable (gcast)
import GHCSpecter.Config
  ( Config (..),
    defaultGhcSpecterConfigFile,
    loadConfig,
  )
import GHCSpecter.Control qualified as Control
import GHCSpecter.Control.Runner
  ( RunnerEnv (..),
    RunnerHandler (..),
  )
import GHCSpecter.Driver.Comm qualified as Comm
import GHCSpecter.Driver.Session qualified as Session (main)
import GHCSpecter.Driver.Session.Types
  ( ClientSession (..),
    HasClientSession (..),
    HasServerSession (..),
    ServerSession (..),
  )
import GHCSpecter.Driver.Worker qualified as Worker
import GHCSpecter.Graphics.DSL
  ( EventMap,
    HitEvent,
    Scene (sceneId),
    Stage (..),
    ViewPort (..),
  )
import GHCSpecter.Gtk.Handler
  ( handleClick,
    handleKeyPressed,
    handleMotion,
    handleScroll,
    handleZoomEnd,
    handleZoomUpdate,
  )
import GHCSpecter.Gtk.Main (renderAction)
import GHCSpecter.Gtk.Types
  ( ViewBackend (..),
    ViewBackendResource (..),
    WrappedViewBackend (..),
  )
import GHCSpecter.Server.Types
  ( initServerState,
  )
import GHCSpecter.UI.Constants
  ( HasWidgetConfig (..),
    appWidgetConfig,
    canvasDim,
    modGraphHeight,
    modGraphWidth,
  )
import GHCSpecter.UI.Types
  ( HasConsoleUI (..),
    HasModuleGraphUI (..),
    HasSessionUI (..),
    HasSourceViewUI (..),
    HasTimingUI (..),
    HasUIModel (..),
    HasUIState (..),
    ViewPortInfo (..),
    emptyUIState,
  )
import GHCSpecter.UI.Types.Event
  ( DetailLevel (..),
    Tab (..),
    UserEvent (..),
  )
import GHCSpecter.Util.Transformation (translateToOrigin)
import GHCSpecter.Util.Transformation qualified as Transformation (hitScene)
import GI.Cairo.Render.Connector qualified as RC
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk
import GI.PangoCairo qualified as PC

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
        { vbrPangoContext = pangoCtxt,
          vbrFontDescSans = descSans,
          vbrFontDescMono = descMono
        }

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
    -- TODO: This should be refactored out.
    let defVP = ViewPort (0, 0) (modGraphWidth, 0.5 * modGraphHeight)
        vpSessionMain =
          appWidgetConfig ^. wcfgSession . at "session-main" . to (maybe defVP translateToOrigin)
        vpSessionProcess =
          appWidgetConfig ^. wcfgSession . at "session-process" . to (maybe defVP translateToOrigin)
        vpSessionRts =
          appWidgetConfig ^. wcfgSession . at "session-rts" . to (maybe defVP translateToOrigin)
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
        vpConsole =
          appWidgetConfig ^. wcfgTopLevel . at "console-main" . to (maybe defVP translateToOrigin)

    let ui0 = emptyUIState assets initTime
        ui0' =
          ui0
            & (uiModel . modelTab .~ TabModuleGraph)
              . ( uiModel . modelSession . sessionUIMainViewPort
                    .~ ViewPortInfo vpSessionMain Nothing
                )
              . ( uiModel . modelSession . sessionUIProcessViewPort
                    .~ ViewPortInfo vpSessionProcess Nothing
                )
              . ( uiModel . modelSession . sessionUIRtsViewPort
                    .~ ViewPortInfo vpSessionRts Nothing
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
              . (uiModel . modelConsole . consoleViewPort .~ ViewPortInfo vpConsole Nothing)

    uiRef <- newTVarIO ui0'
    chanState <- newTChanIO
    chanQEv <- newTQueueIO
    let -- client session
        cliSess = ClientSession uiRef chanState chanQEv
    workQ <- newTQueueIO

    _ <- Gtk.init Nothing
    mvb <- initViewBackendResource
    stageRef <- atomically $ newTVar (Stage [])
    case mvb of
      Nothing -> error "cannot initialize pango"
      Just vbr -> do
        vbRef <- atomically $ do
          emapRef <- newTVar ([] :: [EventMap UserEvent])
          let vb = ViewBackend vbr stageRef emapRef
          newTVar (WrappedViewBackend vb)
        mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
        _ <- mainWindow `on` #destroy $ Gtk.mainQuit
        -- main canvas
        drawingArea <- new Gtk.DrawingArea []
        #addEvents
          drawingArea
          [ Gdk.EventMaskButtonPressMask,
            Gdk.EventMaskButtonReleaseMask,
            Gdk.EventMaskKeyPressMask,
            Gdk.EventMaskKeyReleaseMask,
            Gdk.EventMaskPointerMotionMask,
            Gdk.EventMaskScrollMask,
            Gdk.EventMaskTouchpadGestureMask
          ]

        _ <- drawingArea
          `on` #draw
          $ RC.renderWithContext do
            (vb, ui, ss) <- liftIO $ atomically do
              ui <- readTVar uiRef
              ss <- readTVar ssRef
              emapRef <- newTVar []
              -- TODO: this should not be recreated every time.
              let vb = ViewBackend vbr stageRef emapRef
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
          `after` #scrollEvent
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
        _ <- drawingArea
          `on` #keyPressEvent
          $ \ev -> do
            v <- get ev #keyval
            mtxt <- get ev #string
            print (v, mtxt)
            handleKeyPressed chanQEv (v, mtxt)
            pure True
        #setCanFocus drawingArea True
        #grabFocus drawingArea
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
        let runHandler =
              RunnerHandler
                { runHandlerRefreshAction = refreshAction,
                  runHandlerHitScene = \xy -> atomically $ do
                    WrappedViewBackend vb <- readTVar vbRef
                    let emapRef = vbEventMap vb
                    emaps <- readTVar emapRef
                    let memap = Transformation.hitScene xy emaps
                    pure (join (gcast @_ @(HitEvent UserEvent, ViewPort) <$> memap)),
                  runHandlerGetScene = \name -> atomically $ do
                    WrappedViewBackend vb <- readTVar vbRef
                    let emapRef = vbEventMap vb
                    emaps <- readTVar emapRef
                    let memap = L.find (\emap -> sceneId emap == name) emaps
                    pure (join (gcast @_ @(HitEvent UserEvent, ViewPort) <$> memap)),
                  runHandlerAddToStage = \scene -> atomically $ do
                    WrappedViewBackend vb <- readTVar vbRef
                    Stage cfgs <- readTVar (vbStage vb)
                    let cfgs' = filter (\scene' -> sceneId scene /= sceneId scene') cfgs
                        cfgs'' = scene : cfgs'
                    writeTVar (vbStage vb) (Stage cfgs'')
                }
            runner =
              RunnerEnv
                { runnerCounter = counterRef,
                  runnerUIState = cliSess ^. csUIStateRef,
                  runnerServerState = servSess ^. ssServerStateRef,
                  runnerQEvent = cliSess ^. csPublisherEvent,
                  runnerSignalChan = servSess ^. ssSubscriberSignal,
                  runnerHandler = runHandler
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
        Gtk.main