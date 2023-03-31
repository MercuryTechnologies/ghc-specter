module Types (
  ViewBackend (..),
) where

import GI.Pango qualified as P
import GI.PangoCairo qualified as PC

data ViewBackend = ViewBackend
  { vbPangoContext :: P.Context
  , vbFontDesc :: P.FontDescription
  }
