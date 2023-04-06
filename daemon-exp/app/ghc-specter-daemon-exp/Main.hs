{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkOS, threadDelay)
import Control.Concurrent.STM (
  TChan,
  atomically,
  modifyTVar',
  newTChanIO,
  newTQueueIO,
  newTVar,
  newTVarIO,
  readTChan,
  readTVar,
  retry,
  tryReadTChan,
  writeTChan,
 )
import Control.Lens (to, (%~), (&), (.~), (^.), _1, _2)
import Control.Monad (forever)
import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.GI.Base (AttrOp ((:=)), get, new, on)
import Data.GI.Gtk.Threading (postGUIASync)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
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
  getSS,
  getUI,
  nextEvent,
  printMsg,
  putSS,
  putUI,
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
  timingHeight,
  timingWidth,
 )
import GHCSpecter.UI.Types (
  ExpUI (ExpUI),
  HasExpUI (..),
  HasTimingUI (..),
  HasUIModel (..),
  HasUIState (..),
  HasViewPortInfo (..),
  MainView (..),
  UIState (..),
  UIView (..),
  ViewPort (..),
  ViewPortInfo (..),
  emptyUIState,
 )
import GHCSpecter.UI.Types.Event (
  BackgroundEvent (MessageChanUpdated, RefreshUI),
  ComponentTag (TagBanner, TagModuleGraph, TagTimingView),
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
import Timing (renderTiming)
import Types (ViewBackend (..))
import Util (drawText, transformScroll, transformZoom)

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

renderNotConnected :: ViewBackend -> ExpUI -> R.Render ()
renderNotConnected vb ex = do
  R.save
  -- TODO: refactor this out
  let ViewPort (x0, y0) (x1, y1) =
        fromMaybe
          (ex ^. expViewPortBanner . vpViewPort)
          (ex ^. expViewPortBanner . vpTempViewPort)
      scaleX = (canvasDim ^. _1) / (x1 - x0)
      scaleY = (canvasDim ^. _2) / (y1 - y0)
  R.scale scaleX scaleY
  R.translate (-x0) (-y0)
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
  case mgrvis of
    Nothing -> renderNotConnected vb (ui ^. uiExp)
    Just grVisInfo ->
      case ui ^. uiView of
        MainMode (MainView TabModuleGraph) ->
          renderModuleGraph vb nameMap drvModMap timing clustering grVisInfo
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
controlMain = forever $ do
  printMsg "control tick"
  ev <- nextEvent
  ss <- getSS
  ui <- getUI
  case ev of
    BkgEv MessageChanUpdated -> do
      let ss' = (serverShouldUpdate .~ True) ss
      asyncWork timingWorker
      putSS ss'
    MouseEv TagBanner (Scroll dir' (dx, dy)) -> do
      let ui' =
            ui
              & (uiExp . expViewPortBanner . vpViewPort %~ transformScroll dir' (dx, dy))
                . (uiExp . expViewPortTimingView . vpTempViewPort .~ Nothing)
      putUI ui'
    MouseEv TagTimingView (Scroll dir' (dx, dy)) -> do
      let vp = ui ^. uiModel . modelTiming . timingUIViewPort
          vp' = transformScroll dir' (dx, dy) vp
          ui' =
            ui
              & (uiExp . expViewPortTimingView . vpViewPort .~ vp')
                . (uiExp . expViewPortTimingView . vpTempViewPort .~ Nothing)
                . (uiModel . modelTiming . timingUIViewPort .~ vp')
      putUI ui'
    MouseEv TagBanner (ZoomUpdate (rx, ry) scale) -> do
      let vp = ui ^. uiExp . expViewPortBanner . vpViewPort
          ui' =
            ui
              & (uiExp . expViewPortBanner . vpTempViewPort .~ Just (transformZoom (rx, ry) scale vp))
      putUI ui'
    MouseEv TagTimingView (ZoomUpdate (rx, ry) scale) -> do
      let vp = ui ^. uiExp . expViewPortBanner . vpViewPort
          vp' = (transformZoom (rx, ry) scale vp)
          ui' =
            ui
              & (uiExp . expViewPortBanner . vpTempViewPort .~ Just vp')
                . (uiModel . modelTiming . timingUIViewPort .~ vp')
      putUI ui'
    MouseEv TagBanner ZoomEnd -> do
      let ui' = case ui ^. uiExp . expViewPortBanner . vpTempViewPort of
            Just viewPort ->
              ui
                & ( (uiExp . expViewPortBanner . vpViewPort .~ viewPort)
                      . (uiExp . expViewPortBanner . vpTempViewPort .~ Nothing)
                  )
            Nothing -> ui
      putUI ui'
    MouseEv TagTimingView ZoomEnd -> do
      let ui' = case ui ^. uiExp . expViewPortBanner . vpTempViewPort of
            Just viewPort ->
              ui
                & ( (uiExp . expViewPortBanner . vpViewPort .~ viewPort)
                      . (uiExp . expViewPortBanner . vpTempViewPort .~ Nothing)
                      . (uiModel . modelTiming . timingUIViewPort .~ viewPort)
                  )
            Nothing -> ui
      putUI ui'
    _ -> pure ()
  pure ()

handleScroll :: TChan Event -> ComponentTag -> Gdk.EventScroll -> IO ()
handleScroll chanGtk tag ev = do
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
      writeTChan chanGtk (MouseEv tag (Scroll dir' (dx, dy)))

-- | pinch position in relative coord, i.e. 0 <= x <= 1, 0 <= y <= 1.
handleZoomUpdate :: TChan Event -> ComponentTag -> (Double, Double) -> Double -> IO ()
handleZoomUpdate chanGtk tag (rx, ry) scale =
  atomically $
    writeTChan chanGtk (MouseEv tag (ZoomUpdate (rx, ry) scale))

handleZoomEnd :: TChan Event -> ComponentTag -> IO ()
handleZoomEnd chanGtk tag =
  atomically $
    writeTChan chanGtk (MouseEv tag ZoomEnd)

simpleEventLoop :: TChan Event -> UIChannel -> IO ()
simpleEventLoop chanGtk (UIChannel chanEv chanState chanBkg) = loopM step (BkgEv RefreshUI)
  where
    step ev = do
      atomically $ writeTChan chanEv ev
      (_ui, _ss) <- atomically $ readTChan chanState
      ev' <- atomically $ do
        mbev' <- tryReadTChan chanBkg
        case mbev' of
          Just bev' -> pure (BkgEv bev')
          Nothing -> do
            mev' <- tryReadTChan chanGtk
            case mev' of
              Just ev' -> pure ev'
              Nothing -> retry
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
              . ( uiExp
                    .~ ExpUI
                      ( ViewPortInfo
                          (ViewPort (0, 0) (canvasDim ^. _1, canvasDim ^. _2))
                          Nothing
                      )
                      ( ViewPortInfo
                          (ViewPort (0, 0) (timingWidth, timingHeight))
                          Nothing
                      )
                )
    uiRef <- newTVarIO ui0'
    chanEv <- newTChanIO
    chanState <- newTChanIO
    chanBkg <- newTChanIO
    chanGtk <- newTChanIO
    let
      -- client session
      cliSess = ClientSession uiRef chanEv chanState chanBkg
      -- UIChannel
      uiChan = UIChannel chanEv chanState chanBkg

    workQ <- newTQueueIO

    _ <- Gtk.init Nothing
    mvb <- initViewBackend
    case mvb of
      Nothing -> error "cannot initialize pango"
      Just vb0 -> do
        vbRef <- atomically $ newTVar vb0
        mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
        _ <- mainWindow `on` #destroy $ Gtk.mainQuit
        -- NOTE: we will not use gtk-native widgets at all in the end. this is temporary.
        menuBar <- new Gtk.MenuBar []
        menuitem1 <- Gtk.menuItemNewWithLabel "ModuleGraph"
        _ <- menuitem1 `on` #activate $ do
          atomically $
            modifyTVar' uiRef (uiView .~ MainMode (MainView TabModuleGraph))
          putStrLn "Module Graph is selected"
        menuitem2 <- Gtk.menuItemNewWithLabel "Timing"
        _ <- menuitem2 `on` #activate $ do
          atomically $
            modifyTVar' uiRef (uiView .~ MainMode (MainView TabTiming))
          putStrLn "Timing is selected"
        #append menuBar menuitem1
        #append menuBar menuitem2
        -- main canvas
        drawingArea <- new Gtk.DrawingArea []

        #addEvents
          drawingArea
          [ Gdk.EventMaskScrollMask
          , Gdk.EventMaskTouchpadGestureMask
          ]

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
            -- TODO: this branch will be moved to control
            (ss, ui) <- atomically ((,) <$> readTVar ssRef <*> readTVar uiRef)
            let mgs = ss ^. serverModuleGraphState
                mgrvis = mgs ^. mgsClusterGraph
                tag = case mgrvis of
                  Nothing -> TagBanner
                  Just _ ->
                    case ui ^. uiView of
                      MainMode (MainView TabModuleGraph) -> TagModuleGraph
                      MainMode (MainView TabTiming) -> TagTimingView
                      _ -> TagBanner
            handleScroll chanGtk tag ev
            postGUIASync $
              #queueDraw drawingArea
            pure True

        gzoom <- Gtk.gestureZoomNew drawingArea
        _ <- gzoom
          `on` #scaleChanged
          $ \scale -> do
            -- TODO: this branch will be moved to control
            (ss, ui) <- atomically ((,) <$> readTVar ssRef <*> readTVar uiRef)
            let mgs = ss ^. serverModuleGraphState
                mgrvis = mgs ^. mgsClusterGraph
                tag = case mgrvis of
                  Nothing -> TagBanner
                  Just _ ->
                    case ui ^. uiView of
                      MainMode (MainView TabModuleGraph) -> TagModuleGraph
                      MainMode (MainView TabTiming) -> TagTimingView
                      _ -> TagBanner
            (_, xcenter, ycenter) <- #getBoundingBoxCenter gzoom
            let (width, height) =
                  case tag of
                    TagBanner -> canvasDim
                    TagTimingView -> (timingWidth, timingHeight)
                    _ -> canvasDim
                rx = xcenter / width
                ry = ycenter / height
            handleZoomUpdate chanGtk tag (rx, ry) scale
            postGUIASync $
              #queueDraw drawingArea
        _ <- gzoom
          `on` #end
          $ \_ -> do
            -- TODO: this branch will be moved to control
            (ss, ui) <- atomically ((,) <$> readTVar ssRef <*> readTVar uiRef)
            let mgs = ss ^. serverModuleGraphState
                mgrvis = mgs ^. mgsClusterGraph
                tag = case mgrvis of
                  Nothing -> TagBanner
                  Just _ ->
                    case ui ^. uiView of
                      MainMode (MainView TabModuleGraph) -> TagModuleGraph
                      MainMode (MainView TabTiming) -> TagTimingView
                      _ -> TagBanner
            handleZoomEnd chanGtk tag
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
        _ <- forkOS $ simpleEventLoop chanGtk uiChan
        _ <- forkOS (forceUpdateLoop drawingArea)
        Gtk.main
