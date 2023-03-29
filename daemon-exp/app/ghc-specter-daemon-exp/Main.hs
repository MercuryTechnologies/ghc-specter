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
import Control.Lens (to, (^.))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_, traverse_)
import Data.GI.Base (AttrOp ((:=)), new, on)
import Data.GI.Gtk.Threading (postGUIASync)
import Data.Int (Int32)
import Data.IntMap (IntMap)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Traversable (for)
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
import GHCSpecter.Channel.Outbound.Types (
  ModuleGraphInfo (..),
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
import GHCSpecter.GraphLayout.Types (GraphVisInfo)
import GHCSpecter.Graphics.DSL (Color (..), Primitive (..), TextPosition (..))
import GHCSpecter.Render.Components.GraphView (compileGraphView)
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

setColor :: Color -> R.Render ()
setColor Black = R.setSourceRGBA 0 0 0 1
setColor White = R.setSourceRGBA 1 1 1 1
setColor Red = R.setSourceRGBA 1 0 0 1
setColor Blue = R.setSourceRGBA 0 0 1 1
setColor Green = R.setSourceRGBA 0 0.5 0 1
setColor Gray = R.setSourceRGBA 0.5 0.5 0.5 1
setColor Orange = R.setSourceRGBA 1.0 0.647 0 1 -- FFA500
setColor HoneyDew = R.setSourceRGBA 0.941 1.0 0.941 1 -- F0FFF0
setColor Ivory = R.setSourceRGBA 1.0 1.0 0.941 1 -- FFFFF0
setColor DimGray = R.setSourceRGBA 0.412 0.412 0.412 1 -- 696969

renderPrimitive :: View -> Primitive -> R.Render ()
renderPrimitive _ (Rectangle (x, y) w h mline mbkg mlwidth _) = do
  for_ mbkg $ \bkg -> do
    setColor bkg
    R.rectangle x y w h
    R.fill
  for_ ((,) <$> mline <*> mlwidth) $ \(line, lwidth) -> do
    setColor line
    R.setLineWidth lwidth
    R.rectangle x y w h
    R.stroke
renderPrimitive _ (Polyline start xys end line width) = do
  setColor line
  R.setLineWidth width
  uncurry R.moveTo start
  traverse_ (uncurry R.lineTo) xys
  uncurry R.lineTo end
  R.stroke
renderPrimitive vw (DrawText (x, y) pos color fontSize msg) = do
  let y' = case pos of
        UpperLeft -> y
        LowerLeft -> y - fromIntegral fontSize - 1
  setColor color
  drawText vw (fromIntegral fontSize) (x, y') msg

renderAction ::
  View ->
  IntMap ModuleName ->
  BiKeyMap DriverId ModuleName ->
  KeyMap DriverId Timer ->
  [(Text, [Text])] ->
  Maybe GraphVisInfo ->
  R.Render ()
renderAction vw _ _ _ _ Nothing = do
  R.setSourceRGBA 0 0 0 1
  drawText vw 36 (100, 100) "GHC is not connected yet"
renderAction vw nameMap drvModMap timing clustering (Just grVisInfo) = do
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
      rexp = compileGraphView nameMap valueFor grVisInfo (Nothing, Nothing)
  traverse_ (renderPrimitive vw) rexp

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
            let nameMap =
                  ss ^. serverModuleGraphState . mgsModuleGraphInfo . to mginfoModuleNameMap
                drvModMap = ss ^. serverDriverModuleMap
                timing = ss ^. serverTiming . tsTimingMap
                mgs = ss ^. serverModuleGraphState
                clustering = mgs ^. mgsClustering
                mgrvis = mgs ^. mgsClusterGraph
            renderAction vw nameMap drvModMap timing clustering mgrvis
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
