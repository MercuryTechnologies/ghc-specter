{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.UI.Console
  ( -- * utilities
    getTabName,
    buildEachLine,
    buildTextBlock,

    -- * build component
    buildConsoleTab,
    buildConsoleHelp,
    buildConsoleMain,
    buildConsoleInput,
  )
where

import Control.Lens ((^.), _1)
import Control.Monad (join)
import Data.Foldable qualified as F
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, maybeToList)
import Data.Semigroup (sconcat)
import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Data.Map
  ( IsKey (..),
    KeyMap,
    forwardLookup,
    lookupKey,
  )
import GHCSpecter.Graphics.DSL
  ( Color (..),
    HitEvent (..),
    Primitive (..),
    Scene (..),
    TextFontFace (Mono, Sans),
    TextPosition (..),
    ViewPort (..),
    rectangle,
    viewPortHeight,
    viewPortWidth,
  )
import GHCSpecter.Layouter.Packer
  ( flowLineByLine,
    toSizedLine,
  )
import GHCSpecter.Layouter.Text
  ( MonadTextLayout,
    drawText',
  )
import GHCSpecter.Server.Types
  ( ConsoleItem (..),
    ServerState (..),
  )
import GHCSpecter.UI.Components.Tab
  ( TabConfig (..),
    buildTab,
  )
import GHCSpecter.UI.Constants
  ( canvasDim,
    consoleInputHeight,
  )
import GHCSpecter.UI.Types.Event (ConsoleEvent (..))
import Prelude hiding (div)

getTabName :: ServerState -> DriverId -> Text
getTabName ss k =
  let ktxt = T.pack $ show (unDriverId k)
      mlookedup = forwardLookup k (ss._serverDriverModuleMap)
   in maybe ktxt (\m -> ktxt <> " - " <> m) mlookedup

buildEachLine ::
  forall m e.
  (MonadTextLayout m) =>
  Text ->
  m (ViewPort, NonEmpty (Primitive e))
buildEachLine =
  fmap (toSizedLine . NE.singleton) . drawText' (0, 0) UpperLeft Mono Black 8

buildConsoleTab ::
  forall m k.
  (MonadTextLayout m, IsKey k, Eq k) =>
  [(k, Text)] ->
  Maybe k ->
  m (Scene (Primitive (ConsoleEvent k)))
buildConsoleTab tabs mfocus =
  fmap (fmap ConsoleTab) <$> buildTab tabCfg mfocus
  where
    tabCfg =
      TabConfig
        { tabCfgId = "console-tab",
          tabCfgWidth = canvasDim ^. _1,
          tabCfgHeight = 15,
          tabCfgItems = tabs
        }

buildConsoleHelp ::
  forall m k.
  (MonadTextLayout m) =>
  -- | getHelp. (title, help items), help item: Left: button, Right: text
  (k -> (Text, [Either (Text, ConsoleEvent k) Text])) ->
  Maybe k ->
  m (Scene (Primitive (ConsoleEvent k)))
buildConsoleHelp getHelp mfocus = do
  titleElem <-
    toSizedLine . NE.singleton <$> drawText' (0, 0) UpperLeft Sans Black 8 title
  helpElems <- traverse renderItem items
  let (vp, contentss) = flowLineByLine 0 (titleElem :| helpElems)
      contents = concatMap F.toList $ F.toList contentss
      size = viewPortHeight vp
  pure
    Scene
      { sceneId = "console-help",
        sceneGlobalViewPort = ViewPort (0, 0) (200, size),
        sceneLocalViewPort = ViewPort (0, 0) (200, size),
        sceneElements = contents,
        sceneExtents = Nothing
      }
  where
    mhelp = getHelp <$> mfocus
    (title, items) = fromMaybe ("", []) mhelp
    renderItem (Left (txt, ev)) = do
      let hitEvent =
            HitEvent
              { hitEventHoverOn = Nothing,
                hitEventHoverOff = Nothing,
                hitEventClick = Just (Right ev)
              }
      itm <- drawText' (0, 0) UpperLeft Mono Black 8 txt
      let bbox = primBoundingBox itm
          w = viewPortWidth bbox
          h = viewPortHeight bbox
          rendered =
            rectangle (0, 0) (w + 2) h (Just Black) (Just White) (Just 1.0) (Just hitEvent)
              :| [itm]
      pure $ toSizedLine rendered
    renderItem (Right txt) = buildEachLine txt

buildTextBlock ::
  forall m e.
  (MonadTextLayout m) =>
  Text ->
  m (ViewPort, NonEmpty (Primitive e))
buildTextBlock txt = do
  let ls = T.lines txt
      ls' = fromMaybe (NE.singleton "empty string") (NE.nonEmpty ls)
  ls'' <- traverse buildEachLine ls'
  let (vp, contentss) = flowLineByLine 0 ls''
  pure (vp, sconcat contentss)

{-
-- NOTE: This is obsolete.
-- TODO: place this to the attic.

buildConsoleItem ::
  forall m k.
  (MonadTextLayout m) =>
  ConsoleItem ->
  m (ViewPort, NonEmpty (Primitive (ConsoleEvent k)))
buildConsoleItem (ConsoleCommand txt) = buildEachLine txt
buildConsoleItem (ConsoleText txt) = buildTextBlock txt
buildConsoleItem (ConsoleButton buttonss) = do
  ls :: NonEmpty (ViewPort, NonEmpty (Primitive (ConsoleEvent k))) <-
    case NE.nonEmpty (mapMaybe NE.nonEmpty buttonss) of
      Nothing -> NE.singleton <$> buildEachLine "no buttons"
      Just ls' -> traverse mkRow ls'
  let (vp, contentss) = flowLineByLine 0 ls
      contentss' = sconcat contentss
  pure (vp, contentss')
  where
    mkButton (label, cmd) = do
      let hitEvent =
            HitEvent
              { hitEventHoverOn = Nothing,
                hitEventHoverOff = Nothing,
                hitEventClick = Just (Right (ConsoleButtonPressed False cmd))
              }
      itm <- drawText' (0, 0) UpperLeft Mono Black 8 label
      let bbox = primBoundingBox itm
          w = viewPortWidth bbox
          h = viewPortHeight bbox
      pure $
        rectangle (0, 0) w h (Just Black) (Just White) (Just 1.0) (Just hitEvent)
          :| [itm]
    mkRow :: NonEmpty (Text, Text) -> m (ViewPort, NonEmpty (Primitive (ConsoleEvent k)))
    mkRow buttons = do
      buttons' <- traverse mkButton buttons
      let (vp', placed) = flowInline 0 buttons'
      pure (vp', sconcat placed)
buildConsoleItem (ConsoleCore forest) = buildTextBlock (T.unlines $ fmap render1 forest)
  where
    render1 tr = T.pack $ drawTree $ fmap show tr
-}

buildConsoleMain ::
  forall k.
  (IsKey k, Eq k) =>
  KeyMap k [ConsoleItem] ->
  Maybe k ->
  [ConsoleItem]
buildConsoleMain contents mfocus = items
  where
    mtxts = mfocus >>= (`lookupKey` contents)
    items = join $ maybeToList mtxts

buildConsoleInput ::
  (MonadTextLayout m) =>
  Text ->
  m (Scene (Primitive e))
buildConsoleInput inputEntry = do
  rendered <- drawText' (0, 0) UpperLeft Mono Black 8 inputEntry
  pure
    Scene
      { sceneId = "console-input",
        sceneGlobalViewPort = ViewPort (0, 0) (canvasDim ^. _1, consoleInputHeight),
        sceneLocalViewPort = ViewPort (0, 0) (canvasDim ^. _1, consoleInputHeight),
        sceneElements = [rendered],
        sceneExtents = Nothing
      }
