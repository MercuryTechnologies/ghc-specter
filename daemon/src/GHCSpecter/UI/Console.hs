{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.UI.Console (
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
import Data.Text qualified as T
import Data.Tree (drawTree)
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
  viewPortHeight,
 )
import GHCSpecter.Layouter.Box.Flow (
  flowInline,
  flowLineByLine,
  toSizedLine,
 )
import GHCSpecter.Server.Types (ConsoleItem (..))
import GHCSpecter.UI.Components.Tab (
  TabConfig (..),
  buildTab,
 )
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
  Scene (Primitive (ConsoleEvent k))
buildConsoleTab tabs mfocus = fmap (fmap ConsoleTab) (buildTab tabCfg mfocus)
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
  Scene (Primitive (ConsoleEvent k))
buildConsoleHelp getHelp mfocus =
  Scene
    { sceneId = "console-help"
    , sceneGlobalViewPort = ViewPort (0, 0) (200, size)
    , sceneLocalViewPort = ViewPort (0, 0) (200, size)
    , sceneElements = contents
    , sceneExtent = Nothing
    }
  where
    mhelp = getHelp <$> mfocus
    (title, items) = fromMaybe ("", []) mhelp
    titleElem = toSizedLine $ NE.singleton $ drawText (0, 0) UpperLeft Sans Black 8 title
    renderItem (Left (txt, ev)) =
      let hitEvent =
            HitEvent
              { hitEventHoverOn = Nothing
              , hitEventHoverOff = Nothing
              , hitEventClick = Just (Right ev)
              }
          rendered =
            rectangle (0, 0) 80 10 (Just Black) (Just White) (Just 1.0) (Just hitEvent)
              :| [drawText (0, 0) UpperLeft Mono Black 8 txt]
       in toSizedLine rendered
    renderItem (Right txt) = toSizedLine $ NE.singleton (drawText (0, 0) UpperLeft Mono Gray 8 txt)
    helpElems = fmap renderItem items
    --
    (vp, contentss) = flowLineByLine 0 (titleElem :| helpElems)
    contents = concatMap F.toList $ F.toList contentss
    size = viewPortHeight vp

buildEachLine :: Text -> (ViewPort, NonEmpty (Primitive e))
buildEachLine = toSizedLine . NE.singleton . drawText (0, 0) UpperLeft Mono Black 8

buildTextBlock :: forall e. Text -> (ViewPort, NonEmpty (Primitive e))
buildTextBlock txt =
  let ls = T.lines txt
      ls' = fromMaybe (NE.singleton "empty string") (NE.nonEmpty ls)
      (vp, contentss) =
        flowLineByLine 0 $ fmap buildEachLine ls'
   in (vp, sconcat contentss)

buildConsoleItem ::
  forall k.
  ConsoleItem ->
  (ViewPort, NonEmpty (Primitive (ConsoleEvent k)))
buildConsoleItem (ConsoleCommand txt) =
  toSizedLine $ NE.singleton $ drawText (0, 0) UpperLeft Mono Black 8 txt
buildConsoleItem (ConsoleText txt) =
  buildTextBlock txt
buildConsoleItem (ConsoleButton buttonss) = (vp, contentss')
  where
    mkButton (label, cmd) =
      let hitEvent =
            HitEvent
              { hitEventHoverOn = Nothing
              , hitEventHoverOff = Nothing
              , hitEventClick = Just (Right (ConsoleButtonPressed False cmd))
              }
       in -- TODO: should not have this hard-coded size "120".
          rectangle (0, 0) 120 10 (Just Black) (Just White) (Just 1.0) (Just hitEvent)
            :| [drawText (0, 0) UpperLeft Mono Black 8 label]

    mkRow :: NonEmpty (Text, Text) -> (ViewPort, NonEmpty (Primitive (ConsoleEvent k)))
    mkRow buttons =
      let (vp', placed) = flowInline 0 $ fmap mkButton buttons
       in (vp', sconcat placed)

    ls :: NonEmpty (ViewPort, NonEmpty (Primitive (ConsoleEvent k)))
    ls = case NE.nonEmpty (mapMaybe NE.nonEmpty buttonss) of
      Nothing -> NE.singleton (buildEachLine "no buttons")
      Just ls' -> fmap mkRow ls'
    (vp, contentss) = flowLineByLine 0 ls
    contentss' = sconcat contentss
buildConsoleItem (ConsoleCore forest) = buildTextBlock (T.unlines $ fmap render1 forest)
  where
    render1 tr = T.pack $ drawTree $ fmap show tr

buildConsoleMain ::
  (IsKey k, Eq k) =>
  KeyMap k [ConsoleItem] ->
  Maybe k ->
  Scene (Primitive (ConsoleEvent k))
buildConsoleMain contents mfocus =
  Scene
    { sceneId = "console-main"
    , sceneGlobalViewPort = extent
    , sceneLocalViewPort = extent
    , sceneElements = F.toList $ sconcat rendered
    , sceneExtent = Just extent
    }
  where
    mtxts = mfocus >>= (`lookupKey` contents)
    items = join $ maybeToList mtxts

    contentss = case NE.nonEmpty items of
      Nothing -> NE.singleton $ buildEachLine "No console history"
      Just items' -> fmap buildConsoleItem items'
    (extent, rendered) = flowLineByLine 0 contentss

buildConsoleInput :: Text -> Scene (Primitive e)
buildConsoleInput inputEntry =
  Scene
    { sceneId = "console-input"
    , sceneGlobalViewPort = ViewPort (0, 0) (canvasDim ^. _1, consoleInputHeight)
    , sceneLocalViewPort = ViewPort (0, 0) (canvasDim ^. _1, consoleInputHeight)
    , sceneElements = rendered
    , sceneExtent = Nothing
    }
  where
    rendered = [drawText (0, 0) UpperLeft Mono Black 8 inputEntry]
