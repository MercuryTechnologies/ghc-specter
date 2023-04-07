module Types (
  ViewBackend (..),
) where

import Control.Concurrent.STM (TVar)
import Data.Text (Text)
import GI.Pango qualified as P

data ViewBackend = ViewBackend
  { vbPangoContext :: P.Context
  , vbFontDesc :: P.FontDescription
  , vbEventBoxMap :: TVar [(Text, ((Double, Double), (Double, Double)))]
  }
