{-# LANGUAGE OverloadedLabels #-}

module Main where

import Control (Control, nextEvent, stepControl)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Exception qualified as E
import Control.Monad (forever, replicateM_)
import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable (for_)
import Data.GI.Base (AttrOp ((:=)), get, new, on)
import Data.GI.Gtk.Threading (postGUIASync)
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector (renderWithContext)
import GI.Cairo.Render.Internal qualified as RI
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk
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
  flushDoubleBuffer,
 )
import Types (
  LogcatState,
  emptyLogcatState,
 )
import Log (dumpLog, flushEventQueue)


tickTock :: Gtk.DrawingArea -> R.Surface -> TVar LogcatState -> IO ()
tickTock drawingArea sfc sref = forever $ do
  threadDelay 1_000_000
  flushEventQueue sfc sref
  postGUIASync $
    #queueDraw drawingArea

controlLoop :: Control ()
controlLoop = replicateM_ 100 nextEvent

waitGUIEvent :: MVar () -> IO ()
waitGUIEvent lock = takeMVar lock

main :: IO ()
main = do
  sref <- newTVarIO emptyLogcatState
  lock :: MVar () <- newEmptyMVar
  _ <- Gtk.init Nothing
  mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
  _ <-
    mainWindow `on` #destroy $ do
      liftIO $ putStrLn "I am quitting"
      Gtk.mainQuit
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
  _ <- drawingArea `on` #buttonPressEvent $ \btn -> do
    putMVar lock ()
    putStrLn "----------"
    putStrLn "button pressed"
    btnNum <- get btn #button
    print btnNum
    pure True
  _ <- drawingArea `on` #buttonReleaseEvent $ \btn -> do
    putStrLn "------------"
    putStrLn "button released"
    btnNum <- get btn #button
    print btnNum
    pure True
  _ <- drawingArea `on` #motionNotifyEvent $ \mtn -> do
    putStrLn "--------------"
    putStrLn "motion event"
    mDev <- get mtn #device
    for_ mDev $ \dev -> do
      txt <- #getName dev
      print txt
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
      flip runReaderT sref $
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
