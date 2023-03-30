module Types (
  ViewBackend (..),
  Tab (..),
  ViewModel (..),
) where

import GI.Pango qualified as P
import GI.PangoCairo qualified as PC

data ViewBackend = ViewBackend
  { vbPangoContext :: P.Context
  , vbFontDesc :: P.FontDescription
  }

data Tab = TabModuleGraph | TabTiming

data ViewModel = ViewModel
  { vmCurrentTab :: Tab
  }
