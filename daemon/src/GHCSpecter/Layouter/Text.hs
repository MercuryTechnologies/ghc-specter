module GHCSpecter.Layouter.Text
  ( MonadTextLayout (..),
    drawText',
  )
where

import Data.Functor.Identity (Identity)
import Data.Text (Text)
import GHCSpecter.Graphics.DSL
  ( Color,
    DrawText (..),
    Primitive (..),
    Shape (..),
    TextFontFace,
    TextPosition,
    ViewPort (..),
  )

class (Monad m) => MonadTextLayout m where
  calculateTextDimension ::
    TextFontFace ->
    Int ->
    Text ->
    m (Double, Double)

-- This is a dummy implementation.
-- TODO: at least, this should produce a reasonable size, not fixed 120.
instance MonadTextLayout Identity where
  calculateTextDimension _ font_size _ =
    pure (120, fromIntegral font_size + 3)

drawText' ::
  (MonadTextLayout m) =>
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
