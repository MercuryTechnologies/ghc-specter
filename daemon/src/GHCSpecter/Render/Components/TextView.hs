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

render :: Text -> Widget IHTML a
render txt =
  S.svg
    svgProps
    ( S.style [] [text "text { font-family: monospace; font: 12px monospace; user-select: none; }"] :
      contents
    )
  where
    ls :: [(Int, Text)]
    ls = zip [0 ..] $ T.lines txt
    rowColChars = [((i, j), c) | (i, txt) <- ls, let jcs = zip [0 ..] (T.unpack txt), (j, c) <- jcs]
    nTotal = length ls
    packShow = T.pack . show
    rowSize = 10
    charSize = 8
    topOfBox :: Int -> Int
    topOfBox i = rowSize * i
    leftOfBox :: Int -> Int
    leftOfBox j = charSize * j
    bottomOfBox :: Int -> Int
    bottomOfBox i = rowSize * i + rowSize
    box ((i, j), _) =
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
    mkText (i, txt) =
      S.text
        [ SP.x (packShow $ leftOfBox 0)
        , SP.y (packShow $ bottomOfBox i)
        ]
        [text txt]
    contents = fmap box rowColChars ++ fmap mkText ls

    svgProps =
      [ width (packShow 500)
      , SP.viewBox
          ( "0 0 "
              <> packShow 500
              <> " "
              <> packShow 500
          )
      , SP.version "1.1"
      , xmlns
      ]
