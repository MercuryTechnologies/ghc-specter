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
  writeTQueue,
 )
import Data.Foldable (for_)
import Data.GI.Base (get)
import GHCSpecter.UI.Types.Event (
  Event (..),
  MouseEvent (..),
  ScrollDirection (..),
 )
import GI.Gdk qualified as Gdk

handleMotion ::
  TQueue Event ->
  Gdk.EventMotion ->
  IO ()
handleMotion chanQEv ev = do
  x <- get ev #x
  y <- get ev #y
  atomically $
    writeTQueue chanQEv $
      MouseEv (MouseMove (x, y))

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
