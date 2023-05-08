{-# LANGUAGE OverloadedLabels #-}

module GHCSpecter.Gtk.Types (
  ViewBackendResource (..),
  ViewBackend (..),
  WrappedViewBackend (..),
  GtkRender,
) where

import Control.Concurrent.STM (TVar)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Typeable (Typeable)
import GHCSpecter.Graphics.DSL (
  EventMap,
  Stage,
  TextFontFace (..),
 )
import GHCSpecter.Layouter.Text (
  MonadTextLayout (..),
 )
import GI.Cairo.Render qualified as R
import GI.Pango qualified as P

data ViewBackendResource = ViewBackendResource
  { vbrPangoContext :: P.Context
  , vbrFontDescSans :: P.FontDescription
  , vbrFontDescMono :: P.FontDescription
  }

-- TODO: in the end, stage is an input, and eventmap is an output of a given frame.
-- TODO2: TVar inside TVar is anti-pattern. should be flattened with one TVar.
data ViewBackend e = ViewBackend
  { vbResource :: ViewBackendResource
  , vbStage :: TVar Stage
  , vbEventMap :: TVar [EventMap e]
  }

data WrappedViewBackend = forall e. (Typeable e) => WrappedViewBackend (ViewBackend e)

type GtkRender e = ReaderT (ViewBackend e) R.Render

instance MonadTextLayout (GtkRender e) where
  calculateTextDimension face sz txt = do
    layouter <- vbResource <$> ask
    let ctxt = vbrPangoContext layouter
        desc = case face of
          Sans -> vbrFontDescSans layouter
          Mono -> vbrFontDescMono layouter
    lift $ do
      layout :: P.Layout <- P.layoutNew ctxt
      #setSize desc (fromIntegral sz * P.SCALE)
      #setFontDescription layout (Just desc)
      #setText layout txt (-1)
      (width, height) <- #getPixelSize layout
      pure (fromIntegral width, fromIntegral height)
