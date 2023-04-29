module GHCSpecter.Layouter.Font.Types (
  MonadFontLayout (..),
) where

import Data.Text (Text)
import GHCSpecter.Graphics.DSL (TextFontFace)

class (Monad m) => MonadFontLayout m where
  calculateTextDimension ::
    TextFontFace ->
    Int ->
    Text ->
    m (Double, Double)
