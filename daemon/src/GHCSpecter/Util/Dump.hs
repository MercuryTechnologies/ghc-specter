{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Util.Dump
  ( dumpTiming,
    dumpMemory,
  )
where

import Data.Functor.Identity (runIdentity)
import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.Data.Timing.Types
  ( TimingTable (..),
  )
import GHCSpecter.Graphics.DSL
  ( Color (..),
    DrawText (..),
    Polyline (..),
    Primitive (..),
    Rectangle (..),
    Scene (..),
    Shape (..),
    ViewPort (..),
    viewPortHeight,
    viewPortWidth,
  )
import GHCSpecter.Server.Types
  ( ServerState (..),
    TimingState (..),
  )
import GHCSpecter.UI.Components.TimingView qualified as TimingView
import GHCSpecter.UI.Constants (timingMaxWidth)
import GHCSpecter.UI.Types
  ( TimingUI (..),
    UIModel (..),
    UIState (..),
    ViewPortInfo (..),
  )
import Text.Printf (printf)

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
renderColor Yellow = "yellow"
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

quote :: Text -> Text
quote t = "\"" <> t <> "\""

renderPrimitive ::
  Primitive e ->
  Text
renderPrimitive (Primitive (SRectangle (Rectangle (x, y) w h mline mbkg mlwidth)) _ _mhitEvent) =
  "<rect"
    <> " x="
    <> quote (T.pack $ show x)
    <> " y="
    <> quote (T.pack $ show y)
    <> " width="
    <> quote (T.pack $ show w)
    <> " height="
    <> quote (T.pack $ show h)
    <> " stroke="
    <> quote (maybe "none" renderColor mline)
    <> " fill="
    <> quote (maybe "none" renderColor mbkg)
    <> maybe
      " "
      (\lwidth -> " stroke-width=" <> (quote . T.pack . show) lwidth)
      mlwidth
    <> " />"
renderPrimitive (Primitive (SPolyline (Polyline start xys end color swidth)) _ _) =
  "<polyline"
    <> " points="
    <> quote (makePolylineText (start, end) xys)
    <> " stroke="
    <> quote (renderColor color)
    <> " stroke-width="
    <> quote (T.pack $ show swidth)
    <> " fill="
    <> quote "none"
    <> " />"
renderPrimitive (Primitive (SDrawText (DrawText (x, y) _pos _font color _fontSize msg)) _ _) =
  "<text"
    <> " x="
    <> quote (T.pack $ show x)
    <> " y="
    <> quote (T.pack $ show y)
    -- TODO: proper font size later
    <> " class="
    <> quote "small"
    <> " fill="
    <> quote (renderColor color)
    <> ">"
    <> msg
    <> "</text>"

mkSvg :: ViewPort -> Text -> Text
mkSvg vp contents =
  "<svg"
    <> " width="
    <> quote wtxt
    <> " height="
    <> quote htxt
    <> " view-box="
    <> quote (T.intercalate " " ["0", "0", wtxt, htxt])
    <> " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" >"
    <> "<style>.small { font: 5px sans-serif; } text { user-select: none; }</style>"
    <> "<g>"
    <> contents
    <> "</g>"
    <> "</svg>"
  where
    wtxt = T.pack (show (viewPortWidth vp))
    htxt = T.pack (show (viewPortHeight vp))

dumpTiming :: UIState -> ServerState -> Text
dumpTiming ui ss =
  let drvModMap = ss._serverDriverModuleMap
      tui = ui._uiModel._modelTiming
      ttable = ss._serverTiming._tsTimingTable
      timingInfos = ttable._ttableTimingInfos

      nMods = length timingInfos
      totalHeight = 5 * nMods
      vp = ViewPort (0, 0) (timingMaxWidth, fromIntegral totalHeight)

      tui' =
        tui
          { _timingUIPartition = True,
            _timingUIViewPort = ViewPortInfo vp Nothing
          }

      scene = runIdentity $ TimingView.buildTimingChart drvModMap tui' ttable
      elems = sceneElements scene
      rendered = T.intercalate "\n" (fmap (renderPrimitive) elems)
   in mkSvg vp rendered

dumpMemory :: UIState -> ServerState -> Text
dumpMemory ui ss =
  let drvModMap = ss._serverDriverModuleMap
      tui = ui._uiModel._modelTiming
      ttable = ss._serverTiming._tsTimingTable
      timingInfos = ttable._ttableTimingInfos

      nMods = length timingInfos
      totalHeight = 5 * nMods
      vp = ViewPort (0, 0) (timingMaxWidth, fromIntegral totalHeight)

      tui' =
        tui
          { _timingUIPartition = True,
            _timingUIViewPort = ViewPortInfo vp Nothing
          }

      scene = runIdentity $ TimingView.buildMemChart drvModMap tui' ttable
      elems = sceneElements scene
      rendered = T.intercalate "\n" (fmap (renderPrimitive) elems)
   in mkSvg vp rendered
