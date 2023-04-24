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
import Data.Semigroup (sconcat)
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
  drawText,
  rectangle,
 )
import GHCSpecter.Render.Components.Tab (
  TabConfig (..),
  buildTab,
 )
import GHCSpecter.Render.Components.Util (
  flowInline,
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
    titleElem = drawText (0, 0) UpperLeft Sans Black 8 title
    renderItem (Left (txt, ev)) =
      let hitEvent =
            HitEvent
              { hitEventHoverOn = Nothing
              , hitEventHoverOff = Nothing
              , hitEventClick = Just (Right ev)
              }
       in rectangle (0, 0) 80 10 (Just Black) (Just White) (Just 1.0) (Just hitEvent)
            :| [drawText (0, 0) UpperLeft Mono Black 8 txt]
    renderItem (Right txt) = NE.singleton (drawText (0, 0) UpperLeft Mono Gray 8 txt)
    helpElems = fmap renderItem items
    --
    (size, contentss) = flowLineByLine 0 (NE.singleton titleElem : helpElems)
    contents = concatMap F.toList contentss

buildConsoleItem :: forall k. ConsoleItem -> [Primitive (ConsoleEvent k)]
buildConsoleItem (ConsoleCommand txt) = [drawText (0, 0) UpperLeft Mono Black 8 txt]
buildConsoleItem (ConsoleText txt) = [drawText (0, 0) UpperLeft Mono Black 8 txt]
buildConsoleItem (ConsoleButton buttonss) = concatMap F.toList contentss
  where
    mkButton (label, cmd) =
      let hitEvent =
            HitEvent
              { hitEventHoverOn = Nothing
              , hitEventHoverOff = Nothing
              , hitEventClick = Just (Right (ConsoleButtonPressed False cmd))
              }
       in rectangle (0, 0) 120 10 (Just Black) (Just White) (Just 1.0) (Just hitEvent)
            :| [drawText (0, 0) UpperLeft Mono Black 8 label]

    mkRow :: [(Text, Text)] -> Maybe (NonEmpty (Primitive (ConsoleEvent k)))
    mkRow buttons =
      let placed = snd $ flowInline 0 $ fmap mkButton buttons
       in -- concat the horizontally placed items into a single NonEmpty list
          -- so to group them as a single line.
          sconcat <$> NE.nonEmpty placed

    ls :: [NonEmpty (Primitive (ConsoleEvent k))]
    ls = mapMaybe mkRow buttonss
    (size, contentss) = flowLineByLine 0 ls
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
    rendered = [drawText (0, 0) UpperLeft Mono Black 8 inputEntry]
