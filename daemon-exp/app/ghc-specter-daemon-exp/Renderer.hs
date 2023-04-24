{-# LANGUAGE OverloadedLabels #-}

module Renderer (
  -- * drawing
  setColor,
  drawText,
  renderPrimitive,
  renderScene,

  -- * event map
  addEventMap,
) where

import Control.Concurrent.STM (
  atomically,
  modifyTVar',
 )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Data.Foldable (for_, traverse_)
import Data.Int (Int32)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GHCSpecter.Graphics.DSL (
  Color (..),
  EventMap (..),
  Polyline (..),
  Primitive (..),
  Rectangle (..),
  Scene (..),
  TextFontFace (..),
  TextPosition (..),
  ViewPort (..),
 )
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector qualified as RC
import GI.Pango qualified as P
import GI.PangoCairo qualified as PC
import Types (GtkRender, ViewBackend (..), ViewBackendResource (..))

drawText :: TextFontFace -> Int32 -> (Double, Double) -> Text -> GtkRender e ()
drawText face sz (x, y) msg = do
  vbr <- vbResource <$> ask
  let pangoCtxt = vbrPangoContext vbr
      desc = case face of
        Sans -> vbrFontDescSans vbr
        Mono -> vbrFontDescMono vbr
  lift $ do
    layout :: P.Layout <- P.layoutNew pangoCtxt
    #setSize desc (sz * P.SCALE)
    #setFontDescription layout (Just desc)
    #setText layout msg (-1)
    R.moveTo x y
    ctxt <- RC.getContext
    PC.showLayout ctxt layout

setColor :: Color -> GtkRender e ()
setColor Black = lift $ R.setSourceRGBA 0 0 0 1
setColor White = lift $ R.setSourceRGBA 1 1 1 1
setColor Red = lift $ R.setSourceRGBA 1 0 0 1
setColor Blue = lift $ R.setSourceRGBA 0 0 1 1
setColor Green = lift $ R.setSourceRGBA 0 0.5 0 1
setColor Yellow = lift $ R.setSourceRGBA 1.0 1.0 0 1
setColor Gray = lift $ R.setSourceRGBA 0.5 0.5 0.5 1
setColor Orange = lift $ R.setSourceRGBA 1.0 0.647 0 1 -- FFA500
setColor HoneyDew = lift $ R.setSourceRGBA 0.941 1.0 0.941 1 -- F0FFF0
setColor Ivory = lift $ R.setSourceRGBA 1.0 1.0 0.941 1 -- FFFFF0
setColor DimGray = lift $ R.setSourceRGBA 0.412 0.412 0.412 1 -- 696969
setColor LightGray = lift $ R.setSourceRGBA 0.827 0.827 0.827 1 -- D3D3D3
setColor LightSlateGray = lift $ R.setSourceRGBA 0.467 0.533 0.6 1 -- 778899
setColor RoyalBlue = lift $ R.setSourceRGBA 0.255 0.412 0.882 1 -- 4169E1
setColor DeepSkyBlue = lift $ R.setSourceRGBA 0 0.749 1.0 1 -- 00BFFF
setColor ColorRedLevel0 = lift $ R.setSourceRGBA 1 1 1 1 -- FFFFFF
setColor ColorRedLevel1 = lift $ R.setSourceRGBA 0.992 0.929 0.925 1 -- FDEDEC
setColor ColorRedLevel2 = lift $ R.setSourceRGBA 0.980 0.859 0.847 1 -- FADBD8
setColor ColorRedLevel3 = lift $ R.setSourceRGBA 0.961 0.718 0.694 1 -- F5B7B1
setColor ColorRedLevel4 = lift $ R.setSourceRGBA 0.945 0.580 0.541 1 -- F1948A
setColor ColorRedLevel5 = lift $ R.setSourceRGBA 0.925 0.439 0.388 1 -- EC7063

renderPrimitive :: Primitive e -> GtkRender e ()
renderPrimitive (PRectangle (Rectangle (x, y) w h mline mbkg mlwidth _mname)) = do
  for_ mbkg $ \bkg -> do
    setColor bkg
    lift $ do
      R.rectangle x y w h
      R.fill
  for_ ((,) <$> mline <*> mlwidth) $ \(line, lwidth) -> do
    setColor line
    lift $ do
      R.setLineWidth lwidth
      R.rectangle x y w h
      R.stroke
renderPrimitive (PPolyline (Polyline start xys end line width)) = do
  setColor line
  lift $ do
    R.setLineWidth width
    uncurry R.moveTo start
    traverse_ (uncurry R.lineTo) xys
    uncurry R.lineTo end
    R.stroke
renderPrimitive (DrawText (x, y) pos fontFace color fontSize msg) = do
  let y' = case pos of
        UpperLeft -> y
        LowerLeft -> y - fromIntegral fontSize - 1
  setColor color
  drawText fontFace (fromIntegral fontSize) (x, y') msg

renderScene :: Scene e -> GtkRender e ()
renderScene scene = do
  let ViewPort (cx0, cy0) (cx1, cy1) = sceneGlobalViewPort scene
      ViewPort (vx0, vy0) (vx1, vy1) = sceneLocalViewPort scene
      scaleX = (cx1 - cx0) / (vx1 - vx0)
      scaleY = (cy1 - cy0) / (vy1 - vy0)
  lift $ do
    R.save
    R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
    R.clip
    R.translate cx0 cy0
    R.scale scaleX scaleY
    R.translate (-vx0) (-vy0)
  traverse_ renderPrimitive (sceneElements scene)
  lift R.restore

addEventMap :: Scene e -> GtkRender e ()
addEventMap scene = do
  emapRef <- vbEventMap <$> ask
  let extractEvent (PRectangle (Rectangle (x, y) w h _ _ _ (Just hitEvent))) =
        Just (hitEvent, ViewPort (x, y) (x + w, y + h))
      extractEvent _ = Nothing
      eitms = mapMaybe extractEvent (sceneElements scene)
      emap =
        EventMap
          { eventMapId = sceneId scene
          , eventMapGlobalViewPort = sceneGlobalViewPort scene
          , eventMapLocalViewPort = sceneLocalViewPort scene
          , eventMapElements = eitms
          }
  liftIO $
    atomically $
      modifyTVar' emapRef (\emaps -> emap : emaps)
