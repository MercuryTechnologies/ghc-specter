{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkOS, threadDelay)
import Control.Concurrent.STM (
  atomically,
  newTChanIO,
  newTQueueIO,
  newTVar,
  readTVar,
 )
import Control.Lens ((^.))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.GI.Base (AttrOp ((:=)), new, on)
import Data.GI.Gtk.Threading (postGUIASync)
import Data.Maybe (fromMaybe)
import GHCSpecter.Config (
  Config (..),
  defaultGhcSpecterConfigFile,
  loadConfig,
 )
import GHCSpecter.Driver.Comm qualified as Comm
import GHCSpecter.Driver.Session.Types (ServerSession (..))
import GHCSpecter.GraphLayout.Types (
  Dimension (..),
  GraphVisInfo,
  HasEdgeLayout (..),
  HasGraphVisInfo (..),
  HasNodeLayout (..),
  HasPoint (..),
  Point (..),
 )
import GHCSpecter.Server.Types (
  HasModuleGraphState (..),
  HasServerState (..),
  initServerState,
 )
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector (renderWithContext)
import GI.Gtk qualified as Gtk

withConfig :: Maybe FilePath -> (Config -> IO ()) -> IO ()
withConfig mconfigFile action = do
  let config = fromMaybe defaultGhcSpecterConfigFile mconfigFile
  ecfg <- loadConfig config
  case ecfg of
    Left err -> putStrLn err
    Right cfg -> do
      print cfg
      action cfg

renderAction :: Maybe GraphVisInfo -> R.Render ()
renderAction Nothing = do
  R.setSourceRGBA 0 0 0 1
  R.rectangle 100 100 200 150
  R.fill
renderAction (Just gvi) = do
  for_ (gvi ^. gviNodes) $ \n -> do
    let Point x y = n ^. nodePosition
        Dim w h = n ^. nodeSize
    R.setSourceRGBA 0 0 1 1
    R.rectangle x y w h
    R.stroke
  for_ (gvi ^. gviEdges) $ \e -> do
    let (start, end) = e ^. edgeStartEndPoints
    R.setSourceRGBA 0 0 0 1
    R.setLineWidth 0.5
    R.moveTo (start ^. pointX) (start ^. pointY)
    for_ (e ^. edgeBendPoints) $ \p ->
      R.lineTo (p ^. pointX) (p ^. pointY)
    R.lineTo (end ^. pointX) (end ^. pointY)
    R.stroke

forceUpdateLoop :: Gtk.DrawingArea -> IO ()
forceUpdateLoop drawingArea = forever $ do
  threadDelay 1_000_000
  postGUIASync $
    #queueDraw drawingArea

main :: IO ()
main =
  withConfig Nothing $ \cfg -> do
    let socketFile = configSocket cfg
        nodeSizeLimit = configModuleClusterSize cfg
    ssRef <- atomically $ newTVar (initServerState nodeSizeLimit)
    workQ <- newTQueueIO
    chanSignal <- newTChanIO
    let servSess = ServerSession ssRef chanSignal
    _ <- forkOS $ Comm.listener socketFile servSess workQ

    _ <- Gtk.init Nothing
    mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
    _ <- mainWindow `on` #destroy $ Gtk.mainQuit
    drawingArea <- new Gtk.DrawingArea []
    _ <- drawingArea
      `on` #draw
      $ renderWithContext
      $ do
        ss <- liftIO $ atomically $ readTVar ssRef
        let mgrvis = ss ^. serverModuleGraphState . mgsClusterGraph
        renderAction mgrvis
        pure True
    layout <- do
      vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]
      #packStart vbox drawingArea True True 0
      pure vbox
    #add mainWindow layout
    #setDefaultSize mainWindow 1440 768
    #showAll mainWindow
    _ <- forkOS (forceUpdateLoop drawingArea)
    Gtk.main
