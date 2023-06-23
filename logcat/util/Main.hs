{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (for_)
import Data.Function qualified as Fn (on)
import Data.GI.Base (AttrOp ((:=)), after, get, new, on)
import Data.GI.Gtk.Threading (postGUIASync)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List qualified as L
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (
  diffUTCTime,
  getCurrentTime,
  nominalDiffTimeToSeconds,
 )
import Data.Traversable (for)
import FromHTML qualified as FromHTML
import GHC.RTS.Events (
  Data (events),
  Event (evCap, evSpec, evTime),
  EventInfo (..),
  EventLog (header, dat),
  Timestamp,
 )
import GHCEvents qualified as GHCEvents
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector as RC
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk
import GI.Pango qualified as P
import GI.PangoCairo qualified as PC
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Text.Show.Pretty (pPrint)
import Types (ClosureInfoItem (..))

data ViewPort = ViewPort (Double, Double) (Double, Double)
  deriving (Show)

data GridState = GridState
  { gridViewPort :: ViewPort
  , gridTempViewPort :: Maybe ViewPort
  }
  deriving (Show)

gridInViewPort :: ViewPort -> ([Double], [Double])
gridInViewPort (ViewPort (x0, y0) (x1, y1)) = (xs, ys)
  where
    u0, u1 :: Int
    u0 = floor (x0 / 128.0)
    u1 = floor (x1 / 128.0)
    v0, v1 :: Int
    v0 = floor (y0 / 128.0)
    v1 = floor (y1 / 128.0)
    --
    xs = fmap (fromIntegral . (128 *)) [u0 .. u1]
    ys = fmap (fromIntegral . (128 *)) [v0 .. v1]

textPosInViewPort :: ViewPort -> [(Double, Double)]
textPosInViewPort (ViewPort (x0, y0) (x1, y1)) =
  [(x, y) | x <- xs, y <- ys]
  where
    u0, u1 :: Int
    u0 = floor ((x0 - 120) / 512.0)
    u1 = floor ((x1 - 120) / 512.0)
    v0, v1 :: Int
    v0 = floor ((y0 - 120) / 512.0)
    v1 = floor ((y1 - 120) / 512.0)
    --
    xs = fmap (fromIntegral . (\x -> x * 512 + 120)) [u0 .. u1]
    ys = fmap (fromIntegral . (\y -> y * 512 + 120)) [v0 .. v1]

-- | scroll
transformScroll ::
  Gdk.ScrollDirection ->
  Double ->
  (Double, Double) ->
  ViewPort ->
  ViewPort
transformScroll dir scale (dx, dy) vp = vp'
  where
    ViewPort (x0, y0) (x1, y1) = vp
    dx' = dx / scale
    dy' = dy / scale

    vp' = case dir of
      Gdk.ScrollDirectionRight ->
        ViewPort (x0 + dx', y0) (x1 + dx', y1)
      Gdk.ScrollDirectionLeft ->
        ViewPort (x0 - dx', y0) (x1 - dx', y1)
      Gdk.ScrollDirectionDown ->
        ViewPort (x0, y0 + dy') (x1, y1 + dy')
      Gdk.ScrollDirectionUp ->
        ViewPort (x0, y0 - dy') (x1, y1 - dy')
      Gdk.ScrollDirectionSmooth ->
        ViewPort (x0 + dx', y0 + dy') (x1 + dx', y1 + dy')

-- | zoom
transformZoom ::
  (Double, Double) ->
  Double ->
  ViewPort ->
  ViewPort
transformZoom (rx, ry) scale vp = vp'
  where
    ViewPort (x0, y0) (x1, y1) = vp
    x = x0 + (x1 - x0) * rx
    y = y0 + (y1 - y0) * ry
    x0' = x + (x0 - x) / scale
    y0' = y + (y0 - y) / scale
    x1' = x + (x1 - x) / scale
    y1' = y + (y1 - y) / scale
    vp' = ViewPort (x0', y0') (x1', y1')

drawTextLine :: (P.Context, P.FontDescription) -> (Double, Double) -> Text -> R.Render ()
drawTextLine (pctxt, desc) (x, y) msg = do
  layout :: P.Layout <- P.layoutNew pctxt
  #setSize desc (10 * P.SCALE)
  #setFontDescription layout (Just desc)
  #setText layout msg (-1)
  R.moveTo x y
  ctxt <- RC.getContext
  PC.showLayout ctxt layout

drawTextMultiline ::
  (P.Context, P.FontDescription) ->
  (Double, Double) ->
  [Text] ->
  R.Render ()
drawTextMultiline (ctxt, desc) (x, y) msgs = do
  for_ (zip [y, y + 12 ..] msgs) $ \(y', msg) ->
    drawTextLine (ctxt, desc) (x, y') msg

drawGrid :: ViewPort -> R.Render ()
drawGrid vp@(ViewPort (vx0, vy0) (vx1, vy1)) = do
  R.setSourceRGBA 0 0 0 1.0
  R.setLineWidth 0.1
  let (xs, ys) = gridInViewPort vp
  for_ xs $ \x -> do
    R.moveTo x vy0
    R.lineTo x vy1
    R.stroke
  for_ ys $ \y -> do
    R.moveTo vx0 y
    R.lineTo vx1 y
    R.stroke

drawGraph :: (Double, Double) -> [(Double, Double)] -> R.Render ()
drawGraph _ [] = pure ()
drawGraph (ox, oy) ((x0, y0) : ps) = do
  let getMax vs =
        let vmax0 = maximum vs
         in if vmax0 < 1.0 then 1.0 else vmax0
      xmax = getMax (x0 : fmap fst ps)
      ymax = getMax (y0 : fmap snd ps)
  let xform (x, y) = (ox + x * 200.0 / xmax, oy - y * 20.0 / ymax)
  R.setSourceRGBA 0 0 1.0 1.0
  R.setLineWidth 1.0
  uncurry R.moveTo (xform (x0, y0))
  for_ ps $ \(x, y) ->
    uncurry R.lineTo (xform (x, y))
  R.stroke

drawItem ::
  (P.Context, P.FontDescription) ->
  (Double, Double) ->
  ClosureInfoItem ->
  R.Render ()
drawItem (pctxt, desc) (ox, oy) item = do
  drawGraph (ox, oy) (clsGraph item)
  R.setSourceRGBA 0.3 0.3 0.3 1.0
  drawTextLine (pctxt, desc) (ox + 210, oy - 15.0) (T.pack $ show $ clsN item)
  drawTextLine (pctxt, desc) (ox + 260, oy - 15.0) (T.pack $ show $ clsSize item)
  drawTextLine (pctxt, desc) (ox + 320, oy - 15.0) (clsLabel item)
  drawTextLine (pctxt, desc) (ox + 420, oy - 15.0) (clsDesc item)
  drawTextLine (pctxt, desc) (ox + 700, oy - 15.0) (clsCTy item)
  drawTextLine (pctxt, desc) (ox + 900, oy - 15.0) (clsType item)
  drawTextLine (pctxt, desc) (ox + 1100, oy - 15.0) (clsModule item)
  drawTextLine (pctxt, desc) (ox + 1300, oy - 15.0) (clsLoc item)

myDraw ::
  (P.Context, P.FontDescription, P.FontDescription) ->
  GridState ->
  [ClosureInfoItem] ->
  R.Render ()
myDraw (pangoCtxt, descSans, descMono) s items = do
  let (cx0, cy0) = (0, 0)
      (cx1, cy1) = (1024, 768)
      vp@(ViewPort (vx0, vy0) (vx1, vy1)) =
        fromMaybe (gridViewPort s) (gridTempViewPort s)
      scaleX = (cx1 - cx0) / (vx1 - vx0)
      scaleY = (cy1 - cy0) / (vy1 - vy0)
  R.save
  R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
  R.clip
  --
  R.translate cx0 cy0
  R.scale scaleX scaleY
  R.translate (-vx0) (-vy0)
  --
  drawGrid vp
  for_ (zip [0 ..] items) $ \(i, item) ->
    drawItem (pangoCtxt, descSans) (0, (i + 1) * 30) item

initFont :: IO (Maybe (P.Context, P.FontDescription, P.FontDescription))
initFont = do
  fontMap :: PC.FontMap <- PC.fontMapGetDefault
  pangoCtxt <- #createContext fontMap
  familySans <- #getFamily fontMap "FreeSans"
  mfaceSans <- #getFace familySans Nothing
  familyMono <- #getFamily fontMap "FreeMono"
  mfaceMono <- #getFace familyMono Nothing
  for ((,) <$> mfaceSans <*> mfaceMono) $ \(faceSans, faceMono) -> do
    descSans <- #describe faceSans
    descMono <- #describe faceMono
    pure (pangoCtxt, descSans, descMono)

main :: IO ()
main = do
  args <- getArgs
  eresult <- GHCEvents.extract (args !! 0)
  case eresult of
    Left err -> print err
    Right (l, _) -> do
      let (infos, chunks) = GHCEvents.getChunkedEvents l
          format c =
            c {GHCEvents.chunkSamples = take 3 (GHCEvents.chunkSamples c)}
      mapM_ (pPrint . format) chunks
      mapM_ (pPrint . GHCEvents.chunkStat) $ take 3 chunks
      pPrint $ GHCEvents.constructGraph chunks

main' :: IO ()
main' = do
  args <- getArgs
  dat <- FromHTML.extract (args !! 0)
  let items = take 100 $ L.sortBy (flip compare `Fn.on` clsSize) dat
  mapM_ print items
  ref <- newIORef (GridState (ViewPort (0, 0) (1024, 768)) Nothing)
  _ <- Gtk.init Nothing
  Just (pangoCtxt, descSans, descMono) <- initFont
  mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
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
      start <- R.liftIO $ getCurrentTime
      s <- R.liftIO $ readIORef ref
      myDraw (pangoCtxt, descSans, descMono) s items
      end <- R.liftIO getCurrentTime
      let diff :: Double
          diff = realToFrac $ nominalDiffTimeToSeconds (diffUTCTime end start)
      R.liftIO $ do
        printf "Rendering time: %.5f seconds\n" diff
        hFlush stdout

      pure True
  _ <- drawingArea
    `after` #scrollEvent
    $ \ev -> do
      x <- get ev #x
      y <- get ev #y
      dx <- get ev #deltaX
      dy <- get ev #deltaY
      dir <- get ev #direction
      modifyIORef' ref $ \s ->
        let cx0 = 0
            cx1 = 1024
            vp@(ViewPort (vx0, _) (vx1, _)) = gridViewPort s
            scale = (cx1 - cx0) / (vx1 - vx0)
            vp' = transformScroll dir scale (dx, dy) vp
         in s
              { gridViewPort = vp'
              , gridTempViewPort = Nothing
              }
      postGUIASync (#queueDraw drawingArea)
      pure True
  gzoom <- Gtk.gestureZoomNew drawingArea
  _ <- gzoom
    `on` #scaleChanged
    $ \scale -> do
      (_, x, y) <- #getBoundingBoxCenter gzoom
      modifyIORef' ref $ \s ->
        let vp = gridViewPort s
            (cx0, cy0) = (0, 0)
            (cx1, cy1) = (1024, 768)
            rx = (x - cx0) / (cx1 - cx0)
            ry = (y - cy0) / (cy1 - cy0)
            mtvp = Just $ transformZoom (rx, ry) scale vp
         in s
              { gridTempViewPort = mtvp
              }
      postGUIASync (#queueDraw drawingArea)
  _ <- gzoom
    `on` #end
    $ \_ -> do
      modifyIORef' ref $ \s ->
        let vp = gridViewPort s
            mtvp = gridTempViewPort s
         in s
              { gridViewPort = fromMaybe vp mtvp
              , gridTempViewPort = Nothing
              }
      postGUIASync (#queueDraw drawingArea)
  #setPropagationPhase gzoom Gtk.PropagationPhaseBubble
  layout <- do
    vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]
    #packStart vbox drawingArea True True 0
    pure vbox
  _ <- mainWindow `on` #destroy $ Gtk.mainQuit
  #add mainWindow layout
  #setDefaultSize mainWindow 1024 768
  #showAll mainWindow
  Gtk.main
