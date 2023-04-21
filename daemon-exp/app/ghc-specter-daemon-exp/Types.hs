module Types (
  ViewBackendResource (..),
  ViewBackend (..),
  WrappedViewBackend (..),
  GtkRender,
) where

import Control.Concurrent.STM (TVar)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Typeable (Typeable)
import GHCSpecter.Graphics.DSL (EventMap)
import GHCSpecter.UI.Constants (WidgetConfig)
import GI.Cairo.Render qualified as R
import GI.Pango qualified as P

data ViewBackendResource = ViewBackendResource
  { vbrPangoContext :: P.Context
  , vbrFontDescSans :: P.FontDescription
  , vbrFontDescMono :: P.FontDescription
  }

data ViewBackend e = ViewBackend
  { vbResource :: ViewBackendResource
  , vbWidgetConfig :: WidgetConfig
  , vbEventMap :: TVar [EventMap e]
  }

data WrappedViewBackend = forall e. (Typeable e) => WrappedViewBackend (ViewBackend e)

type GtkRender e = ReaderT (ViewBackend e) R.Render
