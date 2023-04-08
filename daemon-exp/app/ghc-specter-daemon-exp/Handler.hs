{-# LANGUAGE OverloadedLabels #-}

module Handler (
  handleMotion,
  handleScroll,
  handleZoomUpdate,
  handleZoomEnd,
) where

import Control.Concurrent.STM (
  TQueue,
  atomically,
  readTVar,
  writeTQueue,
 )
import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.GI.Base (get)
import Data.List qualified as L
import GHCSpecter.UI.Constants (modGraphHeight, modGraphWidth)
import GHCSpecter.UI.Types (
  HasModuleGraphUI (..),
  HasUIModel (..),
  HasUIState (..),
  HasUIViewRaw (..),
  HasViewPortInfo (..),
  UIState,
  ViewPort (..),
 )
import GHCSpecter.UI.Types.Event (
  Event (..),
  ModuleGraphEvent (..),
  MouseEvent (..),
  ScrollDirection (..),
 )
import GHCSpecter.Util.Transformation (isInside)
import GI.Gdk qualified as Gdk
import Types (ViewBackend (..))

handleMotion ::
  UIState ->
  TQueue Event ->
  Gdk.EventMotion ->
  -- | will redraw?
  IO Bool
handleMotion ui chanQEv ev = do
  x <- get ev #x
  y <- get ev #y
  -- atomically $
  --   writeTQueue chanQEv $ MouseMove

  -- for now. assuming module graph
  let rx = x / modGraphWidth
      ry = y / modGraphHeight
      ViewPort (x0, y0) (x1, y1) = ui ^. uiModel . modelMainModuleGraph . modGraphViewPort . vpViewPort
      x' = x0 + (x1 - x0) * rx
      y' = y0 + (y1 - y0) * ry
      emap = ui ^. uiViewRaw . uiRawEventBoxMap

      mprevHit = ui ^. uiModel . modelMainModuleGraph . modGraphUIHover
      mnowHit = fst <$> L.find (\(_label, box) -> (x', y') `isInside` box) emap
  if (mnowHit /= mprevHit)
    then do
      atomically $ do
        writeTQueue chanQEv $ MainModuleEv (HoverOnModuleEv mnowHit)
      -- TODO: redraw control on control side.
      pure True
    else pure False


handleScroll :: TQueue Event -> Gdk.EventScroll -> IO ()
handleScroll chanQEv ev = do
  dx <- get ev #deltaX
  dy <- get ev #deltaY
  dir <- get ev #direction
  let mdir' = case dir of
        Gdk.ScrollDirectionRight -> Just ScrollDirectionRight
        Gdk.ScrollDirectionLeft -> Just ScrollDirectionLeft
        Gdk.ScrollDirectionDown -> Just ScrollDirectionDown
        Gdk.ScrollDirectionUp -> Just ScrollDirectionUp
        _ -> Nothing
  for_ mdir' $ \dir' -> do
    atomically $ do
      writeTQueue chanQEv (MouseEv (Scroll dir' (dx, dy)))

-- | pinch position in canvas coord
handleZoomUpdate :: TQueue Event -> (Double, Double) -> Double -> IO ()
handleZoomUpdate chanQEv (x, y) scale =
  atomically $
    writeTQueue chanQEv (MouseEv (ZoomUpdate (x, y) scale))

handleZoomEnd :: TQueue Event -> IO ()
handleZoomEnd chanQEv =
  atomically $
    writeTQueue chanQEv (MouseEv ZoomEnd)
