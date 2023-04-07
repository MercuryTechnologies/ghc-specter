{-# LANGUAGE OverloadedLabels #-}

module Util (
  -- * transformation function for viewport
  transformScroll,
  transformZoom,
) where

import Data.Foldable (for_, traverse_)
import Data.Int (Int32)
import Data.Text (Text)
import GHCSpecter.Graphics.DSL (Color (..), Primitive (..), TextPosition (..))
import GHCSpecter.UI.Types (ViewPort (..))
import GHCSpecter.UI.Types.Event (ScrollDirection (..))
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector qualified as RC
import GI.Pango qualified as P
import GI.PangoCairo qualified as PC
import Types (ViewBackend (..))

-- | scroll
transformScroll ::
  ScrollDirection ->
  (Double, Double) ->
  ViewPort ->
  ViewPort
transformScroll dir (dx, dy) (ViewPort (x0, y0) (x1, y1)) =
  case dir of
    ScrollDirectionRight -> ViewPort (x0 + dx, y0) (x1 + dx, y1)
    ScrollDirectionLeft -> ViewPort (x0 - dx, y0) (x1 - dx, y1)
    ScrollDirectionDown -> ViewPort (x0, y0 + dy) (x1, y1 + dy)
    ScrollDirectionUp -> ViewPort (x0, y0 - dy) (x1, y1 - dy)

-- | zoom
transformZoom ::
  (Double, Double) ->
  Double ->
  ViewPort ->
  ViewPort
transformZoom (rx, ry) scale (ViewPort (x0, y0) (x1, y1)) = ViewPort (x0', y0') (x1', y1')
  where
    x = x0 + (x1 - x0) * rx
    y = y0 + (y1 - y0) * ry
    x0' = x + (x0 - x) / scale
    y0' = y + (y0 - y) / scale
    x1' = x + (x1 - x) / scale
    y1' = y + (y1 - y) / scale
