module Types (
  ViewBackend (..),
) where

import GI.Pango qualified as P

data ViewBackend = ViewBackend
  { vbPangoContext :: P.Context
  , vbFontDescSans :: P.FontDescription
  , vbFontDescMono :: P.FontDescription
  }
