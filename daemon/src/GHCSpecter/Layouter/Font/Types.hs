module GHCSpecter.Layouter.Font.Types (
  FontLayouter (..),
) where

import Data.Text (Text)
import GHCSpecter.Graphics.DSL (TextFontFace)

class FontLayouter f where
  withFontEngine :: (f -> IO a) -> IO (Either String a)

  calculateTextDimension ::
    f ->
    TextFontFace ->
    Int ->
    Text ->
    IO (Double, Double)
