{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Util.Dump
  ( dumpTiming,
    dumpMemory,
    dumpModGraph,
  )
where

import Data.Functor.Identity (runIdentity)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.Channel.Outbound.Types (ModuleGraphInfo (..))
import GHCSpecter.Data.Timing.Types
  ( TimingTable (..),
  )
import GHCSpecter.Data.Timing.Util (isModuleCompilationDone)
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
-- import GHCSpecter.Layouter.Graph.Types
--   ( Dimension (..),
--     GraphVisInfo (..),
--   )
import GHCSpecter.Server.Types
  ( ModuleGraphState (..),
    ServerState (..),
    TimingState (..),
  )
import GHCSpecter.UI.Components.GraphView qualified as GraphView
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

mkSvg :: Int -> ViewPort -> Text -> Text
mkSvg padding vp contents =
  "<svg"
    <> " width="
    <> quote (pShow width)
    <> " height="
    <> quote (pShow height)
    <> " viewBox="
    <> quote (T.intercalate " " $ fmap pShow [left, top, right, bottom])
    <> " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" >"
    <> "<style>.small { font: 5px sans-serif; } text { user-select: none; }</style>"
    <> "<g>"
    <> contents
    <> "</g>"
    <> "</svg>"
  where
    pShow = T.pack . show
    left = -padding
    top = -padding
    width = floor (viewPortWidth vp) + 2 * padding
    height = floor (viewPortHeight vp) + 2 * padding
    right = left + width
    bottom = top + height

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
      elems = scene.sceneElements
      rendered = T.intercalate "\n" (fmap (renderPrimitive) elems)
   in mkSvg 0 vp rendered

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

      scene = runIdentity $ TimingView.buildMemChart True 400 drvModMap tui' ttable
      elems = scene.sceneElements
      rendered = T.intercalate "\n" (fmap (renderPrimitive) elems)
   in mkSvg 10 vp rendered

dumpModGraph ::
  ServerState ->
  Text
dumpModGraph ss =
  case mgs._mgsClusterGraph of
    Nothing -> ""
    Just grVisInfo ->
      let -- Dim _canvasWidth canvasHeight = grVisInfo._gviCanvasDim
          scene =
            runIdentity $
              GraphView.buildModuleGraph nameMap valueFor grVisInfo (Nothing, Nothing)
          vp = scene.sceneLocalViewPort
          elems = sceneElements scene
          rendered = T.intercalate "\n" (fmap (renderPrimitive) elems)
       in mkSvg 0 vp rendered
  where
    nameMap = ss._serverModuleGraphState._mgsModuleGraphInfo.mginfoModuleNameMap
    drvModMap = ss._serverDriverModuleMap
    mgs = ss._serverModuleGraphState
    clustering = mgs._mgsClustering
    timing = ss._serverTiming._tsTimingMap
    valueFor name =
      fromMaybe 0 $ do
        cluster <- L.lookup name clustering
        let nTot = length cluster
        if nTot == 0
          then Nothing
          else do
            let compiled = filter (isModuleCompilationDone drvModMap timing) cluster
                nCompiled = length compiled
            pure (fromIntegral nCompiled / fromIntegral nTot)
