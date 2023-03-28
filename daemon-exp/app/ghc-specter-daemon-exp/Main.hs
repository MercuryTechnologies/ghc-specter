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
import Control.Lens ((^.), _1)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_, traverse_)
import Data.GI.Base (AttrOp ((:=)), new, on)
import Data.GI.Gtk.Threading (postGUIASync)
import Data.IntMap qualified as IM
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
  EdgeLayout (..),
  GraphVisInfo,
  HasEdgeLayout (..),
  HasGraphVisInfo (..),
  HasNodeLayout (..),
  HasPoint (..),
  NodeLayout (..),
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
renderAction (Just grVisInfo) = do
  -- TODO: This is largely a copied code from GHCSpecter.Render.Components.GraphView.
  --       This should be refactored out properly.
  let Dim canvasWidth canvasHeight = grVisInfo ^. gviCanvasDim
      nodeLayoutMap =
        IM.fromList $ fmap (\n -> (n ^. nodePayload . _1, n)) (grVisInfo ^. gviNodes)
      -- graph layout parameter
      aFactor = 0.95
      offX = -15
      offYFactor = -1.0
      -- the center of left side of a node
      leftCenter (NodeLayout _ (Point x y) (Dim _ h)) =
        Point (x + offX) (y + h * offYFactor + h + 0.5)
      -- the center of right side of a node
      rightCenter (NodeLayout _ (Point x y) (Dim w h)) =
        Point (x + offX + w * aFactor) (y + h * offYFactor + h + 0.5)
      edge (EdgeLayout _ (src, tgt) (srcPt0, tgtPt0) xys) = do
        let (color, swidth) = ("gray", "1")
            -- if source and target nodes cannot be found,
            -- just use coordinates recorded in edge.
            -- TODO: should be handled as error.
            (srcPt, tgtPt) = fromMaybe (srcPt0, tgtPt0) $ do
              srcNode <- IM.lookup src nodeLayoutMap
              tgtNode <- IM.lookup tgt nodeLayoutMap
              -- Left-to-right flow.
              pure (rightCenter srcNode, leftCenter tgtNode)
        R.setSourceRGBA 0 0 0 1
        R.setLineWidth 0.5
        R.moveTo (srcPt ^. pointX) (srcPt ^. pointY)
        for_ xys $ \p ->
          R.lineTo (p ^. pointX) (p ^. pointY)
        R.lineTo (tgtPt ^. pointX) (tgtPt ^. pointY)
        R.stroke
      node (NodeLayout _ (Point x y) (Dim w h)) = do
        R.setSourceRGBA 0 0 1 1
        R.setLineWidth 0.8
        R.rectangle (x + offX) (y + h * offYFactor + h - 6) (w * aFactor) 13
        R.stroke

  traverse_ node (grVisInfo ^. gviNodes)
  traverse_ edge (grVisInfo ^. gviEdges)

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
