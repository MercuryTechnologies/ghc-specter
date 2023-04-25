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
import GHCSpecter.Render.Components.Tab (
  TabConfig (..),
  buildTab,
 )
import GHCSpecter.Render.Components.Util (
  flowInline,
  flowLineByLine,
  toSizedLine,
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
    (mvp, contentss) = flowLineByLine 0 (titleElem : helpElems)
    contents = concatMap F.toList contentss
    size = maybe 200 viewPortHeight mvp

buildConsoleItem :: forall k. ConsoleItem -> (ViewPort, NonEmpty (Primitive (ConsoleEvent k)))
buildConsoleItem (ConsoleCommand txt) =
  toSizedLine $ NE.singleton $ drawText (0, 0) UpperLeft Mono Black 8 txt
buildConsoleItem (ConsoleText txt) =
  toSizedLine $ NE.singleton $ drawText (0, 0) UpperLeft Mono Black 8 txt
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

    mkRow :: [(Text, Text)] -> Maybe (ViewPort, NonEmpty (Primitive (ConsoleEvent k)))
    mkRow buttons =
      let (mvp, placed) = flowInline 0 $ fmap mkButton buttons
       in -- concat the horizontally placed items into a single NonEmpty list
          -- so to group them as a single line.
          (,) <$> mvp <*> (sconcat <$> NE.nonEmpty placed)

    ls :: [(ViewPort, NonEmpty (Primitive (ConsoleEvent k)))]
    ls = mapMaybe mkRow buttonss
    (mvp, contentss) = flowLineByLine 0 ls
    -- TODO: for now, use this partial function. this should be properly removed.
    Just vp = mvp
    contentss' = NE.fromList (concatMap F.toList contentss)
buildConsoleItem (ConsoleCore forest) = (vp, contents)
  where
    render1 tr =
      let
        -- for debug
        txt = T.pack $ drawTree $ fmap show tr
        {-  ebind = toBind tr
          rendered =
            case ebind of
              Left err -> renderErr (err <> "\n" <> txt)
              Right bind -> renderTopBind bind -}
        ls = T.lines txt
        rendered1 = fmap (toSizedLine . NE.singleton . drawText (0, 0) UpperLeft Mono Black 8) ls
       in
        rendered1
    (mvp, contentss) = flowLineByLine 0 $ concatMap render1 forest
    (vp, contents) = case (,) <$> mvp <*> NE.nonEmpty (concatMap F.toList contentss) of
      Nothing -> toSizedLine $ NE.singleton $ drawText (0, 0) UpperLeft Mono Black 8 "cannot draw core"
      Just (vp_, contents_) -> (vp_, contents_)

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
    (mvp, rendered) = flowLineByLine 0 contentss
    size = maybe 200 viewPortHeight mvp

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
