{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (for_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import GHCSpecter.Graphics.DSL (TextFontFace (..))
import GHCSpecter.Layouter.Font.Types (
  MonadFontLayout (..),
 )
import GI.Pango qualified as P
import GI.PangoCairo qualified as PC

data PangoCairoLayouter = PangoCairoLayouter
  { flContext :: P.Context
  , flDescSans :: P.FontDescription
  , flDescMono :: P.FontDescription
  }

instance MonadFontLayout (ReaderT PangoCairoLayouter IO) where
  calculateTextDimension face sz txt = do
    layouter <- ask
    let ctxt = flContext layouter
        desc = case face of
          Sans -> flDescSans layouter
          Mono -> flDescMono layouter
    lift $ do
      layout :: P.Layout <- P.layoutNew ctxt
      #setSize desc (fromIntegral sz * P.SCALE)
      #setFontDescription layout (Just desc)
      #setText layout txt (-1)
      (width, height) <- #getPixelSize layout
      pure (fromIntegral width, fromIntegral height)

main :: IO ()
main = do
  fontMap :: PC.FontMap <- PC.fontMapGetDefault
  pangoCtxt <- #createContext fontMap
  familySans <- #getFamily fontMap "FreeSans"
  mfaceSans <- #getFace familySans Nothing
  familyMono <- #getFamily fontMap "FreeMono"
  mfaceMono <- #getFace familyMono Nothing
  case ((,) <$> mfaceSans <*> mfaceMono) of
    Nothing -> print "font engine is not initialized well"
    Just (faceSans, faceMono) -> do
      descSans <- #describe faceSans
      descMono <- #describe faceMono
      let layouter =
            PangoCairoLayouter
              { flContext = pangoCtxt
              , flDescSans = descSans
              , flDescMono = descMono
              }
      (x, y) <-
        runReaderT (calculateTextDimension Mono 10 "Hello There") layouter
      print (x, y)
  pure ()
