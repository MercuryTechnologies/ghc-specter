{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Render.Components.Console (
  compileConsoleTab,
  compileConsoleHelp,  
) where

import Control.Lens ((^.), _1)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHCSpecter.Data.Map (
  IsKey (..),
 )
import GHCSpecter.Graphics.DSL (
  Color (..),
  Primitive (..),
  Scene (..),
  TextFontFace (Mono, Sans),
  TextPosition (..),
  ViewPort (..),
 )
import GHCSpecter.Render.Components.Tab (
  TabConfig (..),
  compileTab,
 )
import GHCSpecter.UI.Constants (canvasDim)
import GHCSpecter.UI.Types.Event (ConsoleEvent (..))
import Prelude hiding (div)

compileConsoleTab ::
  (IsKey k, Eq k) =>
  [(k, Text)] ->
  Maybe k ->
  Scene (ConsoleEvent k)
compileConsoleTab tabs mfocus = ConsoleTab <$> compileTab tabCfg mfocus
  where
    tabCfg =
      TabConfig
        { tabCfgId = "console-tab"
        , tabCfgSpacing = 80
        , tabCfgWidth = canvasDim ^. _1
        , tabCfgHeight = 15
        , tabCfgItems = tabs
        }

compileConsoleHelp ::
  -- | getHelp. (title, help items), help item: Left: button, Right: text
  (k -> (Text, [Either (Text, ConsoleEvent k) Text])) ->
  Maybe k ->
  Scene (ConsoleEvent k)
compileConsoleHelp getHelp mfocus =
  Scene
    { sceneId = "console-help"
    , sceneGlobalViewPort = ViewPort (0, 0) (200, size)
    , sceneLocalViewPort = ViewPort (0, 0) (200, size)
    , sceneElements = contents
    }
  where
    mhelp = getHelp <$> mfocus
    (title, items) = fromMaybe ("", []) mhelp
    titleElem = DrawText (0, 0) UpperLeft Sans Black 8 title
    renderItem (Left (txt, _ev)) = DrawText (0, 0) UpperLeft Mono Black 8 txt
    renderItem (Right txt) = DrawText (0, 0) UpperLeft Mono Gray 8 txt
    helpElems = fmap renderItem items
    placing !offset item =
      case item of
        DrawText (x, y) p' ff c fs t ->
          let doffset = fromIntegral fs + 4
           in (offset + doffset, DrawText (x, y + offset) p' ff c fs t)
        Polyline (x0, y0) xs (x1, y1) c w ->
          let doffset = 5
              f (x, y) = (x, y + offset + 3)
           in (offset + doffset, Polyline (f (x0, y0)) (fmap f xs) (f (x1, y1)) c w)
        Rectangle (x, y) w h ms mf mw me ->
          let doffset = h
           in (offset + doffset, Rectangle (x, y + offset) w h ms mf mw me)
    --
    (size, contents) = L.mapAccumL placing 0 (titleElem : helpElems)
