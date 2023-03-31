{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Render.ConcurReplicaSVG (
  makePolylineText,
  renderColor,
  renderPrimitive,
) where

import Concur.Core (Widget)
import Concur.Replica (
  classList,
  height,
  width,
 )
import Concur.Replica.DOM.Props (Props)
import Concur.Replica.SVG.Props qualified as SP
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.Graphics.DSL (Color (..), Primitive (..))
import GHCSpecter.UI.ConcurReplica.DOM (text)
import GHCSpecter.UI.ConcurReplica.SVG qualified as S
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import Text.Printf (printf)
import Prelude hiding (div)

makePolylineText :: ((Double, Double), (Double, Double)) -> [(Double, Double)] -> Text
makePolylineText (p0, p1) xys =
  T.intercalate " " (fmap each ([p0] ++ xys ++ [p1]))
  where
    each (x, y) = T.pack $ printf "%.2f,%.2f" x y

renderColor :: Color -> Text
renderColor Black = "black"
renderColor White = "white"
renderColor Red = "red"
renderColor Blue = "blue"
renderColor Green = "green"
renderColor Gray = "gray"
renderColor Orange = "orange"
renderColor HoneyDew = "honeydew"
renderColor Ivory = "ivory"
renderColor DimGray = "dimgray"
renderColor LightGray = "lightgray"
renderColor LightSlateGray = "lightslategray"
renderColor RoyalBlue = "royalblue"
renderColor DeepSkyBlue = "deepskyblue"
renderColor ColorRedLevel0 = "#FFFFFF"
renderColor ColorRedLevel1 = "#FDEDEC"
renderColor ColorRedLevel2 = "#FADBD8"
renderColor ColorRedLevel3 = "#F5B7B1"
renderColor ColorRedLevel4 = "#F1948A"
renderColor ColorRedLevel5 = "#EC7063"

renderPrimitive :: (Text -> [Props ev]) -> Primitive -> Widget IHTML ev
renderPrimitive handlers (Rectangle (x, y) w h mline mbkg mlwidth handleHover) =
  S.rect
    ( maybe [] (\name -> handlers name) handleHover
        ++ [ SP.x (T.pack $ show x)
           , SP.y (T.pack $ show y)
           , width (T.pack $ show w)
           , height (T.pack $ show h)
           , SP.stroke (maybe "none" renderColor mline)
           , SP.fill (maybe "none" renderColor mbkg)
           , SP.pointerEvents (if isJust handleHover then "visible" else "none")
           ]
        ++ maybe [] (pure . SP.strokeWidth . T.pack . show) mlwidth
    )
    []
renderPrimitive _ (Polyline start xys end color swidth) =
  S.polyline
    [ SP.points (makePolylineText (start, end) xys)
    , SP.stroke (renderColor color)
    , SP.strokeWidth (T.pack $ show swidth)
    , SP.fill "none"
    ]
    []
renderPrimitive _ (DrawText (x, y) _pos color _fontSize msg) =
  S.text
    [ SP.x (T.pack $ show x)
    , SP.y (T.pack $ show y)
    , -- TODO: proper font size later
      classList [("small", True)]
    , SP.fill (renderColor color)
    , SP.pointerEvents "none"
    ]
    [text msg]
