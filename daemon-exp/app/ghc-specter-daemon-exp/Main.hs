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
import Data.Int (Int32)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Traversable (for)
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
import GHCSpecter.Channel.Outbound.Types (
  Timer,
 )
import GHCSpecter.Config (
  Config (..),
  defaultGhcSpecterConfigFile,
  loadConfig,
 )
import GHCSpecter.Data.Map (BiKeyMap, KeyMap)
import GHCSpecter.Data.Timing.Util (isModuleCompilationDone)
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
  HasTimingState (..),
  initServerState,
 )
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector qualified as RC
import GI.Gtk qualified as Gtk
import GI.Pango qualified as P
import GI.PangoCairo qualified as PC

withConfig :: Maybe FilePath -> (Config -> IO ()) -> IO ()
withConfig mconfigFile action = do
  let config = fromMaybe defaultGhcSpecterConfigFile mconfigFile
  ecfg <- loadConfig config
  case ecfg of
    Left err -> putStrLn err
    Right cfg -> do
      print cfg
      action cfg

data View = View
  { viewPangoContext :: P.Context
  , viewFontDesc :: P.FontDescription
  }

initView :: IO (Maybe View)
initView = do
  fontMap :: PC.FontMap <- PC.fontMapGetDefault
  pangoCtxt <- #createContext fontMap
  family <- #getFamily fontMap "FreeSans"
  mface <- #getFace family Nothing
  for mface $ \face -> do
    desc <- #describe face
    pure (View pangoCtxt desc)

drawText :: View -> Int32 -> (Double, Double) -> Text -> R.Render ()
drawText (View pangoCtxt desc) sz (x, y) msg = do
  layout :: P.Layout <- P.layoutNew pangoCtxt
  #setSize desc (sz * P.SCALE)
  #setFontDescription layout (Just desc)
  #setText layout msg (-1)
  R.moveTo x y
  ctxt <- RC.getContext
  PC.showLayout ctxt layout

renderAction ::
  View ->
  BiKeyMap DriverId ModuleName ->
  KeyMap DriverId Timer ->
  [(Text, [Text])] ->
  Maybe GraphVisInfo ->
  R.Render ()
renderAction vw _ _ _ Nothing = do
  R.setSourceRGBA 0 0 0 1
  drawText vw 36 (100, 100) "GHC is not connected yet"
renderAction vw drvModMap timing clustering (Just grVisInfo) = do
  let valueFor name =
        fromMaybe 0 $ do
          cluster <- L.lookup name clustering
          let nTot = length cluster
          if nTot == 0
            then Nothing
            else do
              let compiled = filter (isModuleCompilationDone drvModMap timing) cluster
                  nCompiled = length compiled
              pure (fromIntegral nCompiled / fromIntegral nTot)

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
      node (NodeLayout (_, name) (Point x y) (Dim w h)) = do
        -- base box
        R.setSourceRGBA 1 1 1 1
        R.rectangle (x + offX) (y + h * offYFactor + h - 6) (w * aFactor) 13
        R.fill
        R.setSourceRGBA 0 0 0 1
        R.setLineWidth 0.8
        R.rectangle (x + offX) (y + h * offYFactor + h - 6) (w * aFactor) 13
        R.stroke
        -- module name
        R.setSourceRGBA 0 0 0 1
        let fontSize = 6 :: Int32
        drawText
          vw
          fontSize
          ( x + offX + 2
          , y + h * offYFactor + h {- this -fontSize - 1 is due to text position convention difference -} - (fromIntegral fontSize) - 1
          )
          name
        -- progress bar base box
        R.setSourceRGBA 1 1 1 1
        R.rectangle (x + offX) (y + h * offYFactor + h + 3) (w * aFactor) 4
        R.fill
        R.setSourceRGBA 0 0 0 1
        R.rectangle (x + offX) (y + h * offYFactor + h + 3) (w * aFactor) 4
        R.stroke
        -- progress bar box
        let ratio = valueFor name
            w' = ratio * w
        R.setSourceRGBA 0 0 1 1
        R.rectangle (x + offX) (y + h * offYFactor + h + 3) (w' * aFactor) 4
        R.fill

  traverse_ edge (grVisInfo ^. gviEdges)
  traverse_ node (grVisInfo ^. gviNodes)

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

    _ <- Gtk.init Nothing
    mvw <- initView
    case mvw of
      Nothing -> error "cannot initialize pango"
      Just vw0 -> do
        vwRef <- atomically $ newTVar vw0
        mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
        _ <- mainWindow `on` #destroy $ Gtk.mainQuit
        drawingArea <- new Gtk.DrawingArea []
        _ <- drawingArea
          `on` #draw
          $ RC.renderWithContext
          $ do
            (ss, vw) <- liftIO $ atomically ((,) <$> readTVar ssRef <*> readTVar vwRef)
            let drvModMap = ss ^. serverDriverModuleMap
                timing = ss ^. serverTiming . tsTimingMap
                mgs = ss ^. serverModuleGraphState
                clustering = mgs ^. mgsClustering
                mgrvis = mgs ^. mgsClusterGraph
            renderAction vw drvModMap timing clustering mgrvis
            pure True
        layout <- do
          vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]
          #packStart vbox drawingArea True True 0
          pure vbox
        #add mainWindow layout
        #setDefaultSize mainWindow 1440 768
        #showAll mainWindow
        _ <- forkOS $ Comm.listener socketFile servSess workQ
        _ <- forkOS (forceUpdateLoop drawingArea)
        Gtk.main
