{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Render.Components.Console (
  buildConsoleTab,
  buildConsoleHelp,
  buildConsoleMain,
  buildConsoleInput,
) where

import Control.Lens ((^.), _1)
import Control.Monad (join)
import Data.Foldable qualified as F
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
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
import GHCSpecter.UI.Constants (
  canvasDim,
  consoleInputHeight,
 )
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
       in Rectangle (0, 0) 80 10 (Just Black) (Just White) (Just 1.0) (Just hitEvent)
            :| [DrawText (0, 0) UpperLeft Mono Black 8 txt]
    renderItem (Right txt) = NE.singleton (DrawText (0, 0) UpperLeft Mono Gray 8 txt)
    helpElems = fmap renderItem items
    --
    (size, contentss) = flowLineByLine 0 (NE.singleton titleElem : helpElems)
    contents = concatMap F.toList contentss

buildConsoleItem :: ConsoleItem -> [Primitive e]
buildConsoleItem (ConsoleCommand txt) = [DrawText (0, 0) UpperLeft Mono Black 8 txt]
buildConsoleItem (ConsoleText txt) = [DrawText (0, 0) UpperLeft Mono Black 8 txt]
buildConsoleItem (ConsoleButton buttonss) = concatMap F.toList contentss
  where
    -- TODO: refactor this out (as flow inline)
    {-
    placeHoriz !offset itms =
      let shifted = fmap forEach itms
          itms' = fmap snd shifted
          doffset = maximum (fmap fst shifted)
       in (offset + doffset, itms')
      where
        forEach item =
          case item of
            DrawText (x, y) p' ff c fs t ->
              let doffset = fromIntegral fs + 4
               in (doffset, DrawText (x, y + offset) p' ff c fs t)
            Polyline (x0, y0) xs (x1, y1) c w ->
              let doffset = 5
                  f (x, y) = (x, y + offset + 3)
               in (doffset, Polyline (f (x0, y0)) (fmap f xs) (f (x1, y1)) c w)
            Rectangle (x, y) w h ms mf mw me ->
              let doffset = h
               in (doffset, Rectangle (x, y + offset) w h ms mf mw me)

    -}
    mkButton (label, cmd) =
      Rectangle (0, 0) 120 10 (Just Black) Nothing {- (Just White) -} (Just 1.0) Nothing
        :| [DrawText (0, 0) UpperLeft Mono Black 8 label]
    mkRow buttons = {- placeHoriz 0 $ -} fmap mkButton buttons
    (size, contentss) = flowLineByLine 0 (concatMap mkRow buttonss)
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
    , sceneElements = concatMap F.toList rendered
    }
  where
    mtxts = mfocus >>= (`lookupKey` contents)
    contentss = fmap buildConsoleItem $ join $ maybeToList mtxts
    contentss' = mapMaybe NE.nonEmpty contentss
    (size, rendered) = flowLineByLine 0 contentss'

buildConsoleInput :: Text -> Scene e
buildConsoleInput inputEntry =
  Scene
    { sceneId = "console-input"
    , sceneGlobalViewPort = ViewPort (0, 0) (canvasDim ^. _1, consoleInputHeight)
    , sceneLocalViewPort = ViewPort (0, 0) (canvasDim ^. _1, consoleInputHeight)
    , sceneElements = rendered
    }
  where
    rendered = [DrawText (0, 0) UpperLeft Mono Black 8 inputEntry]
