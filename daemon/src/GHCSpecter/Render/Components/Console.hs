{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Render.Components.Console (
  buildConsoleTab,
  buildConsoleHelp,
  buildConsoleMain,
) where

import Control.Lens ((^.), _1)
import Control.Monad (join)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import GHCSpecter.Data.Map (
  IsKey (..),
  KeyMap,
  lookupKey,
 )
import GHCSpecter.Graphics.DSL (
  Color (..),
  HitEvent (..),
  Primitive (..),
  Scene (..),
  TextFontFace (Mono, Sans),
  TextPosition (..),
  ViewPort (..),
 )
import GHCSpecter.Render.Components.Tab (
  TabConfig (..),
  buildTab,
 )
import GHCSpecter.Render.Components.Util (
  flowLineByLine,
 )
import GHCSpecter.Server.Types (ConsoleItem (..))
import GHCSpecter.UI.Constants (canvasDim)
import GHCSpecter.UI.Types.Event (ConsoleEvent (..))
import Prelude hiding (div)

buildConsoleTab ::
  (IsKey k, Eq k) =>
  [(k, Text)] ->
  Maybe k ->
  Scene (ConsoleEvent k)
buildConsoleTab tabs mfocus = ConsoleTab <$> buildTab tabCfg mfocus
  where
    tabCfg =
      TabConfig
        { tabCfgId = "console-tab"
        , tabCfgSpacing = 150
        , tabCfgWidth = canvasDim ^. _1
        , tabCfgHeight = 15
        , tabCfgItems = tabs
        }

buildConsoleHelp ::
  -- | getHelp. (title, help items), help item: Left: button, Right: text
  (k -> (Text, [Either (Text, ConsoleEvent k) Text])) ->
  Maybe k ->
  Scene (ConsoleEvent k)
buildConsoleHelp getHelp mfocus =
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
    renderItem (Left (txt, ev)) =
      let hitEvent =
            HitEvent
              { hitEventHoverOn = Nothing
              , hitEventHoverOff = Nothing
              , hitEventClick = Just (Right ev)
              }
       in [ Rectangle (0, 0) 80 10 (Just Black) (Just White) (Just 1.0) (Just hitEvent)
          , DrawText (0, 0) UpperLeft Mono Black 8 txt
          ]
    renderItem (Right txt) = [DrawText (0, 0) UpperLeft Mono Gray 8 txt]
    helpElems = fmap renderItem items
    --
    (size, contentss) = flowLineByLine 0 ([titleElem] : helpElems)
    contents = concat contentss

buildConsoleItem :: ConsoleItem -> [Primitive e]
buildConsoleItem (ConsoleCommand txt) = [DrawText (0, 0) UpperLeft Mono Black 8 txt]
buildConsoleItem (ConsoleText txt) = [DrawText (0, 0) UpperLeft Mono Black 8 txt]
buildConsoleItem (ConsoleButton buttonss) = []
buildConsoleItem (ConsoleCore forest) = []

buildConsoleMain ::
  (IsKey k, Eq k) =>
  KeyMap k [ConsoleItem] ->
  Maybe k ->
  Scene (ConsoleEvent k)
buildConsoleMain contents mfocus =
  Scene
    { sceneId = "console-main"
    , sceneGlobalViewPort = ViewPort (0, 0) (canvasDim ^. _1, size)
    , sceneLocalViewPort = ViewPort (0, 0) (canvasDim ^. _1, size)
    , sceneElements = concat rendered
    }
  where
    mtxts = mfocus >>= (`lookupKey` contents)
    (size, rendered) =
      flowLineByLine 0 (fmap buildConsoleItem (join (maybeToList mtxts)))
