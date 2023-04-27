{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (for_)
import GHCSpecter.Graphics.DSL (TextFontFace (..))
import GHCSpecter.Layouter.Font.Types (
  FontLayouter (..),
 )
import GI.Pango qualified as P
import GI.PangoCairo qualified as PC

data PangoCairoLayouter = PangoCairoLayouter
  { flContext :: P.Context
  , flDescSans :: P.FontDescription
  , flDescMono :: P.FontDescription
  }

instance FontLayouter PangoCairoLayouter where
  withFontEngine action = do
    fontMap :: PC.FontMap <- PC.fontMapGetDefault
    pangoCtxt <- #createContext fontMap
    familySans <- #getFamily fontMap "FreeSans"
    mfaceSans <- #getFace familySans Nothing
    familyMono <- #getFamily fontMap "FreeMono"
    mfaceMono <- #getFace familyMono Nothing
    case ((,) <$> mfaceSans <*> mfaceMono) of
      Nothing -> pure (Left "font engine is not initialized well")
      Just (faceSans, faceMono) -> do
        descSans <- #describe faceSans
        descMono <- #describe faceMono
        let layouter =
              PangoCairoLayouter
                { flContext = pangoCtxt
                , flDescSans = descSans
                , flDescMono = descMono
                }
        Right <$> action layouter
  calculateTextDimension layouter face sz txt = do
    let ctxt = flContext layouter
        desc = case face of
          Sans -> flDescSans layouter
          Mono -> flDescMono layouter
    layout :: P.Layout <- P.layoutNew ctxt
    #setSize desc (fromIntegral sz * P.SCALE)
    #setFontDescription layout (Just desc)
    #setText layout txt (-1)
    (width, height) <- #getPixelSize layout
    pure (fromIntegral width, fromIntegral height)

main :: IO ()
main = do
  withFontEngine @PangoCairoLayouter $ \layouter -> do
    (x, y) <- calculateTextDimension layouter Mono 10 "Hello There"
    print (x, y)
  pure ()
