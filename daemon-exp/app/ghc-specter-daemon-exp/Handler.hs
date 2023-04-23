{-# LANGUAGE OverloadedLabels #-}

module Handler (
  handleClick,
  handleMotion,
  handleScroll,
  handleZoomUpdate,
  handleZoomEnd,
  handleKeyPressed,
) where

import Control.Concurrent.STM (
  TQueue,
  atomically,
  writeTQueue,
 )
import Data.Foldable (for_)
import Data.GI.Base (get)
import Data.Text (Text)
import Data.Word (Word32)
import GHCSpecter.UI.Types.Event (
  Event (..),
  KeyEvent (..),
  MouseEvent (..),
  ScrollDirection (..),
 )
import GI.Gdk qualified as Gdk

handleClick ::
  TQueue Event ->
  Gdk.EventButton ->
  IO ()
handleClick chanQEv ev = do
  x <- get ev #x
  y <- get ev #y
  atomically $
    writeTQueue chanQEv $
      MouseEv (MouseClick (x, y))

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
  x <- get ev #x
  y <- get ev #y
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
      writeTQueue chanQEv (MouseEv (Scroll dir' (x, y) (dx, dy)))

-- | pinch position in canvas coord
handleZoomUpdate :: TQueue Event -> (Double, Double) -> Double -> IO ()
handleZoomUpdate chanQEv (x, y) scale =
  atomically $
    writeTQueue chanQEv (MouseEv (ZoomUpdate (x, y) scale))

handleZoomEnd :: TQueue Event -> IO ()
handleZoomEnd chanQEv =
  atomically $
    writeTQueue chanQEv (MouseEv ZoomEnd)

handleKeyPressed :: TQueue Event -> (Word32, Maybe Text) -> IO ()
handleKeyPressed chanQEv (v, mtxt) = do
  atomically $ do
    if v == 13
      then writeTQueue chanQEv (KeyEv SpecialKeyEnter)
      else for_ mtxt $ \txt -> writeTQueue chanQEv (KeyEv (NormalKeyPressed txt))
