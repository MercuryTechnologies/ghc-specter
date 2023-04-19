module Types (
  ViewBackend (..),
  GtkRender,
) where

import Control.Concurrent.STM (TVar)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)
import GHCSpecter.Graphics.DSL (EventMap)
import GHCSpecter.UI.Types (UIState)
import GI.Cairo.Render qualified as R
import GI.Pango qualified as P

data ViewBackend = ViewBackend
  { vbPangoContext :: P.Context
  , vbFontDescSans :: P.FontDescription
  , vbFontDescMono :: P.FontDescription
  , vbEventMap :: TVar [EventMap Text]
  }

type GtkRender = ReaderT (ViewBackend, TVar UIState) R.Render
