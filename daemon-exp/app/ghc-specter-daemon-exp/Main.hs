{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkOS, threadDelay)
import Control.Concurrent.STM (
  TQueue,
  atomically,
  flushTQueue,
  newTChanIO,
  newTQueueIO,
  newTVar,
  newTVarIO,
  readTChan,
  readTVar,
  retry,
  writeTChan,
  writeTQueue,
 )
import Control.Lens (to, (&), (.~), (^.), _1, _2)
import Control.Monad (forever)
import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_, traverse_)
import Data.GI.Base (AttrOp ((:=)), get, new, on)
import Data.GI.Gtk.Threading (postGUIASync)
import Data.List (partition)
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
  asyncWork,
  modifyUI,
  modifyUISS,
  nextEvent,
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
import GHCSpecter.UI.Constants (
  canvasDim,
  modGraphHeight,
  modGraphWidth,
  timingHeight,
  timingWidth,
 )
import GHCSpecter.UI.Types (
  HasModuleGraphUI (..),
  HasTimingUI (..),
  HasUIModel (..),
  HasUIState (..),
  HasViewPortInfo (..),
  MainView (..),
  UIState (..),
  UIView (..),
  ViewPortInfo (..),
  emptyUIState,
 )
import GHCSpecter.UI.Types.Event (
  BackgroundEvent (MessageChanUpdated, RefreshUI),
  Event (..),
  MouseEvent (..),
  ScrollDirection (..),
  Tab (..),
 )
import GHCSpecter.Worker.Timing (timingWorker)
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector qualified as RC
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk
import GI.PangoCairo qualified as PC
import ModuleGraph (renderModuleGraph)
import Renderer (drawText)
import Timing (renderTiming)
import Types (ViewBackend (..))
import Util (transformScroll, transformZoom)

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
  R.save
  R.setSourceRGBA 0 0 0 1
  drawText vb 36 (100, 100) "GHC is not connected yet"
  R.restore

renderAction ::
  ViewBackend ->
  ServerState ->
  UIState ->
  R.Render ()
renderAction vb ss ui = do
  let nameMap =
        ss ^. serverModuleGraphState . mgsModuleGraphInfo . to mginfoModuleNameMap
      drvModMap = ss ^. serverDriverModuleMap
      timing = ss ^. serverTiming . tsTimingMap
      mgs = ss ^. serverModuleGraphState
      clustering = mgs ^. mgsClustering
      mgrvis = mgs ^. mgsClusterGraph
      mgrui = ui ^. uiModel . modelMainModuleGraph
  case mgrvis of
    Nothing -> renderNotConnected vb
    Just grVisInfo ->
      case ui ^. uiView of
        MainMode (MainView TabModuleGraph) ->
          renderModuleGraph vb mgrui nameMap drvModMap timing clustering grVisInfo
        MainMode (MainView TabTiming) -> do
          let tui = ui ^. uiModel . modelTiming
              ttable = ss ^. serverTiming . tsTimingTable
          renderTiming vb drvModMap tui ttable
        _ -> pure ()

forceUpdateLoop :: Gtk.DrawingArea -> IO ()
forceUpdateLoop drawingArea = forever $ do
  threadDelay 1_000_000
  postGUIASync $
    #queueDraw drawingArea

controlMain :: Control ()
controlMain = nextEvent >>= goModuleGraph

goModuleGraph :: Event -> Control ()
goModuleGraph ev = do
  case ev of
    BkgEv MessageChanUpdated -> do
      modifyUISS (\(ui, ss) -> let ss' = (serverShouldUpdate .~ True) ss in (ui, ss'))
      asyncWork timingWorker
      ev' <- nextEvent
      goModuleGraph ev'
    TabEv TabTiming -> do
      modifyUI $ uiView .~ MainMode (MainView TabTiming)
      ev' <- nextEvent
      goTiming ev'
    MouseEv (Scroll dir' (dx, dy)) -> do
      modifyUISS $ \(ui, ss) ->
        let vp = ui ^. uiModel . modelMainModuleGraph . modGraphViewPort . vpViewPort
            vp' = transformScroll dir' (dx, dy) vp
            ui' =
              ui
                & (uiModel . modelMainModuleGraph . modGraphViewPort .~ ViewPortInfo vp' Nothing)
         in (ui', ss)
      ev' <- nextEvent
      goModuleGraph ev'
    MouseEv (ZoomUpdate (xcenter, ycenter) scale) -> do
      modifyUISS $ \(ui, ss) ->
        let vp = ui ^. uiModel . modelMainModuleGraph . modGraphViewPort . vpViewPort
            rx = xcenter / modGraphWidth
            ry = ycenter / modGraphHeight
            vp' = (transformZoom (rx, ry) scale vp)
            ui' =
              ui
                & (uiModel . modelMainModuleGraph . modGraphViewPort . vpTempViewPort .~ Just vp')
         in (ui', ss)
      ev' <- nextEvent
      goModuleGraph ev'
    MouseEv ZoomEnd -> do
      modifyUISS $ \(ui, ss) ->
        let ui' = case ui ^. uiModel . modelMainModuleGraph . modGraphViewPort . vpTempViewPort of
              Just viewPort ->
                ui
                  & (uiModel . modelMainModuleGraph . modGraphViewPort .~ ViewPortInfo viewPort Nothing)
              Nothing -> ui
         in (ui', ss)
      ev' <- nextEvent
      goModuleGraph ev'
    _ -> do
      ev' <- nextEvent
      goModuleGraph ev'

goTiming :: Event -> Control ()
goTiming ev = do
  case ev of
    BkgEv MessageChanUpdated -> do
      modifyUISS (\(ui, ss) -> let ss' = (serverShouldUpdate .~ True) ss in (ui, ss'))
      asyncWork timingWorker
      ev' <- nextEvent
      goTiming ev'
    TabEv TabModuleGraph -> do
      modifyUI $ uiView .~ MainMode (MainView TabModuleGraph)
      ev' <- nextEvent
      goModuleGraph ev'
    MouseEv (Scroll dir' (dx, dy)) -> do
      modifyUISS $ \(ui, ss) ->
        let vp = ui ^. uiModel . modelTiming . timingUIViewPort . vpViewPort
            vp' = transformScroll dir' (dx, dy) vp
            ui' =
              ui
                & (uiModel . modelTiming . timingUIViewPort .~ ViewPortInfo vp' Nothing)
         in (ui', ss)
      ev' <- nextEvent
      goTiming ev'
    MouseEv (ZoomUpdate (xcenter, ycenter) scale) -> do
      modifyUISS $ \(ui, ss) ->
        let vp = ui ^. uiModel . modelTiming . timingUIViewPort . vpViewPort
            rx = xcenter / timingWidth
            ry = ycenter / timingHeight
            vp' = (transformZoom (rx, ry) scale vp)
            ui' =
              ui
                & (uiModel . modelTiming . timingUIViewPort . vpTempViewPort .~ Just vp')
         in (ui', ss)
      ev' <- nextEvent
      goTiming ev'
    MouseEv ZoomEnd -> do
      modifyUISS $ \(ui, ss) ->
        let ui' = case ui ^. uiModel . modelTiming . timingUIViewPort . vpTempViewPort of
              Just viewPort ->
                ui
                  & (uiModel . modelTiming . timingUIViewPort .~ ViewPortInfo viewPort Nothing)
              Nothing -> ui
         in (ui', ss)
      ev' <- nextEvent
      goTiming ev'
    _ -> do
      ev' <- nextEvent
      goTiming ev'

handleScroll :: TQueue Event -> Gdk.EventScroll -> IO ()
handleScroll chanQEv ev = do
  dx <- get ev #deltaX
  dy <- get ev #deltaY
  dir <- get ev #direction
  let mdir' = case dir of
        Gdk.ScrollDirectionRight -> Just ScrollDirectionRight
        Gdk.ScrollDirectionLeft -> Just ScrollDirectionLeft
        Gdk.ScrollDirectionDown -> Just ScrollDirectionDown
        Gdk.ScrollDirectionUp -> Just ScrollDirectionUp
        _ -> Nothing
  for_ mdir' $ \dir' -> do
    atomically $ do
      writeTQueue chanQEv (MouseEv (Scroll dir' (dx, dy)))

-- | pinch position in relative coord, i.e. 0 <= x <= 1, 0 <= y <= 1.
handleZoomUpdate :: TQueue Event -> (Double, Double) -> Double -> IO ()
handleZoomUpdate chanQEv (rx, ry) scale =
  atomically $
    writeTQueue chanQEv (MouseEv (ZoomUpdate (rx, ry) scale))

handleZoomEnd :: TQueue Event -> IO ()
handleZoomEnd chanQEv =
  atomically $
    writeTQueue chanQEv (MouseEv ZoomEnd)

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
    let ui0 = emptyUIState assets initTime
        ui0' =
          ui0
            & (uiModel . modelTiming . timingUIPartition .~ True)
              . (uiModel . modelTiming . timingUIHowParallel .~ False)
              . (uiView .~ MainMode (MainView TabModuleGraph))
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
    mvb <- initViewBackend
    case mvb of
      Nothing -> error "cannot initialize pango"
      Just vb0 -> do
        vbRef <- atomically $ newTVar vb0
        mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
        _ <- mainWindow `on` #destroy $ Gtk.mainQuit
        -- main canvas
        drawingArea <- new Gtk.DrawingArea []
        #addEvents
          drawingArea
          [ Gdk.EventMaskScrollMask
          , Gdk.EventMaskTouchpadGestureMask
          ]

        -- NOTE: we will not use gtk-native widgets at all in the end. this is temporary.
        menuBar <- new Gtk.MenuBar []
        menuitem1 <- Gtk.menuItemNewWithLabel "ModuleGraph"
        _ <- menuitem1 `on` #activate $ do
          atomically $ writeTQueue chanQEv (TabEv TabModuleGraph)
          -- atomically $
          --   modifyTVar' uiRef (uiView .~ MainMode (MainView TabModuleGraph))
          -- putStrLn "Module Graph is selected"
          postGUIASync $
            #queueDraw drawingArea

        menuitem2 <- Gtk.menuItemNewWithLabel "Timing"
        _ <- menuitem2 `on` #activate $ do
          atomically $ writeTQueue chanQEv (TabEv TabTiming)
          postGUIASync $
            #queueDraw drawingArea

        -- atomically $
        --   modifyTVar' uiRef (uiView .~ MainMode (MainView TabTiming))
        -- putStrLn "Timing is selected"
        #append menuBar menuitem1
        #append menuBar menuitem2

        _ <- drawingArea
          `on` #draw
          $ RC.renderWithContext
          $ do
            (vb, ss, ui) <- liftIO $ atomically ((,,) <$> readTVar vbRef <*> readTVar ssRef <*> readTVar uiRef)
            renderAction vb ss ui
            pure True
        _ <- drawingArea
          `on` #scrollEvent
          $ \ev -> do
            handleScroll chanQEv ev
            postGUIASync $
              #queueDraw drawingArea
            pure True

        gzoom <- Gtk.gestureZoomNew drawingArea
        _ <- gzoom
          `on` #scaleChanged
          $ \scale -> do
            (_, xcenter, ycenter) <- #getBoundingBoxCenter gzoom
            handleZoomUpdate chanQEv (xcenter, ycenter) scale
            postGUIASync $
              #queueDraw drawingArea
        _ <- gzoom
          `on` #end
          $ \_ -> do
            handleZoomEnd chanQEv
            postGUIASync $
              #queueDraw drawingArea
        #setPropagationPhase gzoom Gtk.PropagationPhaseBubble

        layout <- do
          vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]
          #packStart vbox menuBar False True 0
          #packStart vbox drawingArea True True 0
          pure vbox
        #add mainWindow layout
        #setDefaultSize mainWindow (canvasDim ^. _1) (canvasDim ^. _2)
        #showAll mainWindow

        _ <- forkOS $ Comm.listener socketFile servSess workQ
        _ <- forkOS $ Session.main servSess cliSess controlMain
        _ <- forkOS $ simpleEventLoop uiChan
        _ <- forkOS (forceUpdateLoop drawingArea)
        Gtk.main
