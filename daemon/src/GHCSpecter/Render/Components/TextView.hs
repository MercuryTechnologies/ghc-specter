{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Render.Components.TextView (
  -- * predefined layout
  rowSize,
  ratio,
  charSize,
  topOfBox,
  bottomOfBox,
  leftOfBox,
  rightOfBox,

  -- * build
  buildTextView,
) where

import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.Graphics.DSL (
  Color (..),
  Primitive (..),
  Scene (..),
  TextFontFace (Mono),
  TextPosition (..),
  ViewPort (..),
  rectangle,
 )

-- TODO: generalize and refactor out these layout parameters
rowSize :: Double
rowSize = 8

ratio :: Double
ratio = 0.625

charSize :: Double
charSize = rowSize * ratio

topOfBox :: Int -> Double
topOfBox i = rowSize * fromIntegral (i - 1)

bottomOfBox :: Int -> Double
bottomOfBox i = rowSize * fromIntegral i

leftOfBox :: Int -> Double
leftOfBox j = charSize * fromIntegral (j - 1)

rightOfBox :: Int -> Double
rightOfBox j = charSize * fromIntegral j

buildTextView :: Text -> [((Int, Int), (Int, Int))] -> Scene e
buildTextView txt highlighted =
  Scene
    { sceneId = "text-view"
    , sceneGlobalViewPort = ViewPort (0, 0) (totalWidth, fromIntegral nTotal * rowSize)
    , sceneLocalViewPort = ViewPort (0, 0) (totalWidth, fromIntegral nTotal * rowSize)
    , sceneElements = contents
    }
  where
    -- NOTE: Rows and columns are 1-based following the GHC convention.
    ls :: [(Int, Text)]
    ls = zip [1 ..] $ T.lines txt
    nTotal = length ls
    totalWidth =
      case ls of
        [] -> 200
        _ -> charSize * fromIntegral (maximum $ fmap (T.length . snd) ls)
    boxSize ((startI, startJ), (endI, endJ)) =
      let w1 = charSize * fromIntegral (endJ - startJ + 1)
          h1 = rowSize * fromIntegral (endI - startI + 1)
       in (w1, h1 + 2)
    highlightBox range@((startI, startJ), _) =
      rectangle
        (leftOfBox startJ, topOfBox startI)
        (fst (boxSize range))
        (snd (boxSize range))
        (Just Red)
        (Just Yellow)
        (Just 1.0)
        Nothing
    mkText (i, t) =
      DrawText (leftOfBox 1, bottomOfBox i) LowerLeft Mono Black 6 t
    contents =
      fmap highlightBox highlighted
        ++ fmap mkText ls
