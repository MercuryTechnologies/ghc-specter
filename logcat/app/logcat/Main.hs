{-# LANGUAGE OverloadedLabels #-}

module Main where

import Control (Control, modifyState, nextEvent, stepControl)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Exception qualified as E
import Control.Lens ((&), (.~), (^.))
import Control.Monad (forever)
import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.GI.Base (AttrOp ((:=)), get, new, on)
import Data.GI.Gtk.Threading (postGUIASync)
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector (renderWithContext)
import GI.Cairo.Render.Internal qualified as RI
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk
import Log (dumpLog, flushEventQueue)
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
import Types (
  CEvent (..),
  HasLogcatState (..),
  HasViewState (..),
  LogcatState,
  emptyLogcatState,
 )
import View (computeLabelPositions, hitTest)

tickTock :: Gtk.DrawingArea -> R.Surface -> TVar LogcatState -> IO ()
tickTock drawingArea sfc sref = forever $ do
  threadDelay 1_000_000
  flushEventQueue sfc sref
  postGUIASync $
    #queueDraw drawingArea

hoverHighlight :: (Double, Double) -> LogcatState -> LogcatState
hoverHighlight (x, y) s =
  let vs = s ^. logcatViewState
      posMap = vs ^. viewLabelPositions
   in (logcatViewState . viewHitted .~ hitTest (x, y) posMap) s

controlLoop :: Control ()
controlLoop =
  forever $ do
    MotionNotify (x, y) <- nextEvent
    modifyState (hoverHighlight (x, y))

waitGUIEvent :: MVar CEvent -> IO CEvent
waitGUIEvent lock = takeMVar lock

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
  drawingArea <- new Gtk.DrawingArea []
  sF :: Double <- fromIntegral <$> #getScaleFactor drawingArea
  -- NOTE: this should be closed with surfaceFinish
  sfc <- R.createImageSurface R.FormatARGB32 (floor (canvasWidth * sF)) (floor (canvasHeight * sF))
  RI.surfaceSetDeviceScale sfc sF sF
  _ <- drawingArea
    `on` #draw
    $ renderWithContext
    $ do
      flushDoubleBuffer sfc
      pure True
  _ <- drawingArea `on` #motionNotifyEvent $ \mtn -> do
    x <- get mtn #x
    y <- get mtn #y
    putMVar lock (MotionNotify (x, y))
    R.renderWith sfc $ do
      drawLogcatState sref
    postGUIASync $
      #queueDraw drawingArea
    -- hoverHighlight drawingArea sfc sref (x, y)
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

  _ <- forkIO $ tickTock drawingArea sfc sref
  _ <- forkIO $ receiver sref
  _ <-
    forkIO $
      flip runReaderT (lock, sref) $
        loopM (\c -> liftIO (waitGUIEvent lock) >> stepControl c) controlLoop
  Gtk.main
  R.surfaceFinish sfc

receiver :: TVar LogcatState -> IO ()
receiver sref =
  withSocketsDo $ do
    let file = "/tmp/eventlog.sock"
        open = do
          sock <- socket AF_UNIX Stream 0
          connect sock (SockAddrUnix file)
          pure sock
    E.bracket open close (dumpLog sref)
    pure ()
