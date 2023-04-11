{-# LANGUAGE OverloadedLabels #-}

module Renderer (
  -- * drawing
  setColor,
  drawText,
  renderPrimitive,
  renderScene,

  -- * reset
  resetWidget,

  -- * event map
  addEventMap,
) where

import Control.Concurrent.STM (
  TVar,
  atomically,
  modifyTVar',
  readTVar,
 )
import Control.Lens ((%~), (.~), (^.))
import Data.Foldable (for_, traverse_)
import Data.Int (Int32)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GHCSpecter.Graphics.DSL (
  Color (..),
  EventMap (..),
  Primitive (..),
  Scene (..),
  TextPosition (..),
  ViewPort (..),
 )
import GHCSpecter.UI.Types (
  HasUIModel (..),
  HasUIState (..),
  HasUIViewRaw (..),
  HasWidgetConfig (..),
  UIState,
 )
import GHCSpecter.UI.Types.Event (Tab (..))
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector qualified as RC
import GI.Pango qualified as P
import GI.PangoCairo qualified as PC
import Types (ViewBackend (..))

drawText :: ViewBackend -> Int32 -> (Double, Double) -> Text -> R.Render ()
drawText vb sz (x, y) msg = do
  let pangoCtxt = vbPangoContext vb
      desc = vbFontDesc vb
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
setColor LightGray = R.setSourceRGBA 0.827 0.827 0.827 1 -- D3D3D3
setColor LightSlateGray = R.setSourceRGBA 0.467 0.533 0.6 1 -- 778899
setColor RoyalBlue = R.setSourceRGBA 0.255 0.412 0.882 1 -- 4169E1
setColor DeepSkyBlue = R.setSourceRGBA 0 0.749 1.0 1 -- 00BFFF
setColor ColorRedLevel0 = R.setSourceRGBA 1 1 1 1 -- FFFFFF
setColor ColorRedLevel1 = R.setSourceRGBA 0.992 0.929 0.925 1 -- FDEDEC
setColor ColorRedLevel2 = R.setSourceRGBA 0.980 0.859 0.847 1 -- FADBD8
setColor ColorRedLevel3 = R.setSourceRGBA 0.961 0.718 0.694 1 -- F5B7B1
setColor ColorRedLevel4 = R.setSourceRGBA 0.945 0.580 0.541 1 -- F1948A
setColor ColorRedLevel5 = R.setSourceRGBA 0.925 0.439 0.388 1 -- EC7063

renderPrimitive :: ViewBackend -> Primitive -> R.Render ()
renderPrimitive _ (Rectangle (x, y) w h mline mbkg mlwidth _mname) = do
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
renderPrimitive vb (DrawText (x, y) pos color fontSize msg) = do
  let y' = case pos of
        UpperLeft -> y
        LowerLeft -> y - fromIntegral fontSize - 1
  setColor color
  drawText vb (fromIntegral fontSize) (x, y') msg

renderScene :: ViewBackend -> Scene -> R.Render ()
renderScene vb scene = do
  let ViewPort (cx0, cy0) (cx1, cy1) = sceneGlobalViewPort scene
      ViewPort (vx0, vy0) (vx1, vy1) = sceneLocalViewPort scene
      scaleX = (cx1 - cx0) / (vx1 - vx0)
      scaleY = (cy1 - cy0) / (vy1 - vy0)
  R.save
  R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
  R.clip
  R.translate cx0 cy0
  R.scale scaleX scaleY
  R.translate (-vx0) (-vy0)
  traverse_ (renderPrimitive vb) (sceneElements scene)
  R.restore

resetWidget :: TVar UIState -> IO (Map Text ViewPort)
resetWidget uiRef = do
  atomically $ do
    modifyTVar' uiRef (uiViewRaw . uiRawEventMap .~ [])
    model <- (^. uiModel) <$> readTVar uiRef
    case model ^. modelTab of
      TabSession -> pure (model ^. modelWidgetConfig . wcfgSession)
      TabModuleGraph -> pure (model ^. modelWidgetConfig . wcfgModuleGraph)
      TabSourceView -> pure (model ^. modelWidgetConfig . wcfgSourceView)
      TabTiming -> pure (model ^. modelWidgetConfig . wcfgTiming)

addEventMap :: TVar UIState -> Scene -> IO ()
addEventMap uiRef scene = do
  let extractEvent (Rectangle (x, y) w h _ _ _ (Just name)) = Just (name, ViewPort (x, y) (x + w, y + h))
      extractEvent _ = Nothing
      eitms = mapMaybe extractEvent (sceneElements scene)
      emap =
        EventMap (sceneGlobalViewPort scene) (sceneLocalViewPort scene) eitms
  atomically $
    modifyTVar' uiRef $
      uiViewRaw . uiRawEventMap %~ (\emaps -> emap : emaps)
