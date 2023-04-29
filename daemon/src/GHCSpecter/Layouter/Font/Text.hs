module GHCSpecter.Layouter.Font.Text (
  drawText',
) where

import Data.Text (Text)
import GHCSpecter.Graphics.DSL (
  Color,
  DrawText (..),
  Primitive (..),
  Shape (..),
  TextFontFace,
  TextPosition,
  ViewPort (..),
 )
import GHCSpecter.Layouter.Font.Types (MonadFontLayout (..))

drawText' ::
  (MonadFontLayout m) =>
  (Double, Double) ->
  TextPosition ->
  TextFontFace ->
  Color ->
  Int ->
  Text ->
  m (Primitive e)
drawText' (x, y) text_pos font_face font_color font_size txt = do
  (w, h) <- calculateTextDimension font_face font_size txt
  pure $
    Primitive
      (SDrawText $ DrawText (x, y) text_pos font_face font_color font_size txt)
      (ViewPort (x, y) (x + w, y + h))
      Nothing
