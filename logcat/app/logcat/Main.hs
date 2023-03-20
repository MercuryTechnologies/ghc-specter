{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control (Control, nextEvent, stepControl, updateState, updateView)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Exception qualified as E
import Control.Lens ((&), (.~), (^.))
import Control.Monad (forever, void, when)
import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.GI.Base (AttrOp ((:=)), get, new, on)
import Data.GI.Gtk.Threading (postGUIASync)
import Data.Traversable (for)
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector (renderWithContext)
import GI.Cairo.Render.Internal qualified as RI
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk
import GI.Pango qualified as P
import GI.PangoCairo qualified as PC
import Log (dumpLog, flushEventQueue, recordEvent)
import Network.Socket (
  Family (AF_UNIX),
  SockAddr (SockAddrUnix),
  SocketType (Stream),
  close,
  connect,
  socket,
  withSocketsDo,
 )
import Render (
  canvasHeight,
  canvasWidth,
  drawLogcatState,
  flushDoubleBuffer,
  xoffset,
  yoffset,
 )
import System.Exit (exitFailure)
import Types (
  CEvent (..),
  HasLogcatState (..),
  HasLogcatView (..),
  HasViewState (..),
  LogcatState,
  LogcatView (..),
  emptyLogcatState,
 )
import View (computeLabelPositions, hitTest)

receiver :: MVar CEvent -> IO ()
receiver lock =
  withSocketsDo $ do
    let file = "/tmp/eventlog.sock"
        open = do
          sock <- socket AF_UNIX Stream 0
          connect sock (SockAddrUnix file)
          pure sock
    E.bracket open close (dumpLog (putMVar lock . RecordEvent) (putMVar lock . UpdateBytes))
    pure ()

tickTock :: MVar CEvent -> IO ()
tickTock lock = forever $ do
  threadDelay 1_000_000
  putMVar lock FlushEventQueue

hoverHighlight :: (Double, Double) -> LogcatState -> (Bool, LogcatState)
hoverHighlight (x, y) s =
  let vs = s ^. logcatViewState
      posMap = vs ^. viewLabelPositions
      hitted = hitTest (x, y) posMap
      shouldUpdate = hitted /= vs ^. viewHitted
      s' = (logcatViewState . viewHitted .~ hitTest (x, y) posMap) s
   in (shouldUpdate, s')

waitGUIEvent :: MVar CEvent -> IO CEvent
waitGUIEvent lock = takeMVar lock

controlLoop :: Control ()
controlLoop =
  forever $ do
    ev <- nextEvent
    case ev of
      MotionNotify (x, y) -> do
        shouldUpdate <- updateState (hoverHighlight (x, y))
        when shouldUpdate updateView
      FlushEventQueue -> do
        shouldUpdate <- updateState flushEventQueue
        when shouldUpdate updateView
      RecordEvent e -> do
        let upd s = (False, recordEvent e s)
        void $ updateState upd
      UpdateBytes nBytes -> do
        let upd s =
              let s' = s & (logcatEventlogBytes .~ nBytes)
               in (False, s')
        void $ updateState upd

initPangoContextAndFontDesc :: IO (Maybe (P.Context, P.FontDescription))
initPangoContextAndFontDesc = do
  fontMap :: PC.FontMap <- PC.fontMapGetDefault
  pangoCtxt <- #createContext fontMap
  family <- #getFamily fontMap "FreeSans"
  mface <- #getFace family Nothing
  for mface $ \face -> do
    desc <- #describe face
    pure (pangoCtxt, desc)

initDrawingAreaAndView :: TVar LogcatState -> IO (Gtk.DrawingArea, LogcatView)
initDrawingAreaAndView sref = do
  drawingArea <- new Gtk.DrawingArea []
  sF :: Double <- fromIntegral <$> #getScaleFactor drawingArea
  -- NOTE: this should be closed with surfaceFinish
  sfc <- R.createImageSurface R.FormatARGB32 (floor (canvasWidth * sF)) (floor (canvasHeight * sF))
  RI.surfaceSetDeviceScale sfc sF sF
  let updater view = do
        R.renderWith sfc $ do
          drawLogcatState view sref
        postGUIASync $
          #queueDraw drawingArea
  mpctxtDesc <- initPangoContextAndFontDesc
  case mpctxtDesc of
    Nothing -> exitFailure
    Just (pctxt, desc) -> do
      let view =
            LogcatView
              { _logcatViewSurface = sfc
              , _logcatViewPangoContext = pctxt
              , _logcatViewFontDesc = desc
              , _logcatViewUpdater = updater
              }
      pure (drawingArea, view)

main :: IO ()
main = do
  let initState =
        emptyLogcatState
          & (logcatViewState . viewLabelPositions .~ computeLabelPositions (xoffset, yoffset))
  sref <- newTVarIO initState
  lock <- newEmptyMVar
  _ <- Gtk.init Nothing
  mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
  _ <- mainWindow `on` #destroy $ Gtk.mainQuit
  (drawingArea, lcView) <- initDrawingAreaAndView sref
  _ <- drawingArea
    `on` #draw
    $ renderWithContext
    $ do
      flushDoubleBuffer (lcView ^. logcatViewSurface)
      pure True
  _ <- drawingArea `on` #motionNotifyEvent $ \mtn -> do
    x <- get mtn #x
    y <- get mtn #y
    putMVar lock (MotionNotify (x, y))
    pure True
  #addEvents
    drawingArea
    [ Gdk.EventMaskButtonPressMask
    , Gdk.EventMaskButtonReleaseMask
    , Gdk.EventMaskPointerMotionMask
    ]
  layout <- do
    vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]
    #packStart vbox drawingArea True True 0
    pure vbox
  #add mainWindow layout
  #setDefaultSize mainWindow (floor canvasWidth) (floor canvasHeight)
  #showAll mainWindow

  _ <- forkIO $ tickTock lock
  _ <- forkIO $ receiver lock
  _ <-
    forkIO $
      flip runReaderT (lock, sref, lcView) $
        loopM (\c -> liftIO (waitGUIEvent lock) >> stepControl c) controlLoop
  Gtk.main
  R.surfaceFinish (lcView ^. logcatViewSurface)
