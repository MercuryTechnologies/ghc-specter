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

  -- * top-level render function
  render,
) where

import Concur.Core (Widget)
import Concur.Replica (
  height,
  width,
 )
import Concur.Replica.SVG.Props qualified as SP
import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.Render.Util (xmlns)
import GHCSpecter.UI.ConcurReplica.DOM (text)
import GHCSpecter.UI.ConcurReplica.SVG qualified as S
import GHCSpecter.UI.ConcurReplica.Types (IHTML)

rowSize :: Double
rowSize = 8

ratio :: Double
ratio = 0.6

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

render :: Bool -> Text -> [((Int, Int), (Int, Int))] -> Widget IHTML a
render showCharBox txt highlighted =
  -- NOTE: white-space: pre to preserve white-space occurrences in the source code.
  S.svg
    svgProps
    ( S.style [] [text "text { font: 8px monospace; user-select: none; white-space: pre; }"]
        : contents
    )
  where
    -- NOTE: Rows and columns are 1-based following the GHC convention.
    ls :: [(Int, Text)]
    ls = zip [1 ..] $ T.lines txt
    rowColChars = [((i, j), c) | (i, t) <- ls, let jcs = zip [1 ..] (T.unpack t), (j, c) <- jcs]
    nTotal = length ls
    totalWidth =
      case ls of
        [] -> 200
        _ -> charSize * fromIntegral (maximum $ fmap (T.length . snd) ls)
    packShow = T.pack . show
    charBox ((i, j), _) =
      S.rect
        [ SP.x (packShow $ leftOfBox j)
        , SP.y (packShow $ topOfBox i)
        , SP.width (packShow charSize)
        , SP.height (packShow rowSize)
        , SP.stroke "gray"
        , SP.strokeWidth "0.25"
        , SP.fill "none"
        ]
        []

    boxSize ((startI, startJ), (endI, endJ)) =
      let w1 = charSize * fromIntegral (endJ - startJ + 1)
          h1 = rowSize * fromIntegral (endI - startI + 1)
       in (w1, h1 + 2)

    highlightBox range@((startI, startJ), _) =
      S.rect
        [ SP.x (packShow $ leftOfBox startJ)
        , SP.y (packShow $ topOfBox startI)
        , SP.width (packShow $ fst (boxSize range))
        , SP.height (packShow $ snd (boxSize range))
        , SP.fill "yellow"
        ]
        []
    highlightBox2 range@((startI, startJ), _) =
      S.rect
        [ SP.x (packShow $ leftOfBox startJ)
        , SP.y (packShow $ topOfBox startI)
        , SP.width (packShow $ fst (boxSize range))
        , SP.height (packShow $ snd (boxSize range))
        , SP.stroke "red"
        , SP.strokeWidth "1px"
        , SP.fill "none"
        ]
        []

    mkText (i, t) =
      S.text
        [ SP.x (packShow $ leftOfBox 1)
        , SP.y (packShow $ bottomOfBox i)
        ]
        [text t]

    contents =
      let contents_ =
            fmap highlightBox highlighted
              ++ fmap mkText ls
              ++ fmap highlightBox2 highlighted
       in if showCharBox
            then fmap charBox rowColChars ++ contents_
            else contents_

    svgProps =
      [ width (packShow totalWidth)
      , height (packShow (fromIntegral nTotal * rowSize))
      , SP.version "1.1"
      , xmlns
      ]
