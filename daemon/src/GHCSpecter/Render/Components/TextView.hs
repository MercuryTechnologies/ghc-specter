module GHCSpecter.Render.Components.TextView
  ( render,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( classList,
    height,
    onClick,
    onInput,
    onMouseEnter,
    onMouseLeave,
    style,
    width,
  )
import Concur.Replica.SVG.Props qualified as SP
import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.Render.Util (xmlns)
import GHCSpecter.UI.ConcurReplica.DOM
  ( div,
    input,
    label,
    pre,
    text,
  )
import GHCSpecter.UI.ConcurReplica.SVG qualified as S
import GHCSpecter.UI.ConcurReplica.Types (IHTML)

render :: Bool -> Text -> [((Int, Int), (Int, Int))] -> Widget IHTML a
render showCharBox txt highlighted =
  S.svg
    svgProps
    ( S.style [] [text "text { font: 8px monospace; user-select: none; }"] :
      contents
    )
  where
    -- NOTE: Rows and columns are 1-based following the GHC convention.
    ls :: [(Int, Text)]
    ls = zip [1 ..] $ T.lines txt
    rowColChars = [((i, j), c) | (i, txt) <- ls, let jcs = zip [1 ..] (T.unpack txt), (j, c) <- jcs]
    nTotal = length ls
    totalWidth =
      case ls of
        [] -> 200
        _ -> charSize * fromIntegral (maximum $ fmap (T.length . snd) ls)
    packShow = T.pack . show
    rowSize = 8
    ratio = 0.6
    charSize = rowSize * ratio
    topOfBox :: Int -> Double
    topOfBox i = rowSize * fromIntegral (i - 1)
    bottomOfBox :: Int -> Double
    bottomOfBox i = rowSize * fromIntegral i
    leftOfBox :: Int -> Double
    leftOfBox j = charSize * fromIntegral (j - 1)
    rightOfBox :: Int -> Double
    rightOfBox j = charSize * fromIntegral j
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
    highlightBox ((startI, startJ), (endI, endJ)) =
      S.rect
        [ SP.x (packShow $ leftOfBox startJ)
        , SP.y (packShow $ topOfBox startI)
        , SP.width (packShow (charSize * fromIntegral (endJ - startJ + 1)))
        , SP.height (packShow (rowSize * fromIntegral (endI - startI + 1)))
        , SP.fill "yellow"
        ]
        []

    mkText (i, txt) =
      S.text
        [ SP.x (packShow $ leftOfBox 1)
        , SP.y (packShow $ bottomOfBox i)
        ]
        [text txt]
    contents =
      if showCharBox
        then
          fmap charBox rowColChars
            ++ fmap highlightBox highlighted
            ++ fmap mkText ls
        else
          fmap mkText ls
            ++ fmap highlightBox highlighted
            ++ fmap mkText ls

    svgProps =
      [ width (packShow totalWidth)
      , height (packShow (fromIntegral nTotal * rowSize))
      , SP.version "1.1"
      , xmlns
      ]