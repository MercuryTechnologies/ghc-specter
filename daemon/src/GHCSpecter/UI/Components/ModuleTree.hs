{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.UI.Components.ModuleTree
  ( buildModuleTree,
    expandableText,
  )
where

import Data.Bifunctor (first)
import Data.List qualified as L
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import Data.Tree (Tree (..), flatten, foldTree)
import GHCSpecter.Channel.Common.Types (ModuleName)
import GHCSpecter.Data.Timing.Util (isModuleCompilationDone)
import GHCSpecter.Graphics.DSL
  ( Color (..),
    HitEvent (..),
    Primitive (..),
    Scene (..),
    TextFontFace (..),
    TextPosition (..),
    ViewPort (..),
    movePrimitiveBy,
    rectangle,
    viewPortHeight,
    viewPortSum,
    viewPortWidth,
  )
import GHCSpecter.Layouter.Text
  ( MonadTextLayout (..),
    drawText',
  )
import GHCSpecter.Server.Types
  ( ModuleGraphState (..),
    ServerState (..),
    TimingState (..),
  )
import GHCSpecter.UI.Constants (canvasDim)
import GHCSpecter.UI.Types
  ( SourceViewUI (..),
  )
import GHCSpecter.UI.Types.Event (SourceViewEvent (..))
import GHCSpecter.Util.SourceTree
  ( accumPrefix,
    expandFocusOnly,
    markLeaf,
  )

expandableText :: Bool -> Bool -> Text -> Text
expandableText isBordered isExpandable txt =
  let txt'
        | not isBordered && isExpandable = txt <> " ... "
        | otherwise = txt
   in txt'

buildModuleTree ::
  forall m.
  (MonadTextLayout m) =>
  SourceViewUI ->
  ServerState ->
  m (Scene (Primitive SourceViewEvent))
buildModuleTree srcUI ss = do
  rendered0 <-
    for displayedForest' $ \tr -> do
      tr' <- traverse renderNode tr
      pure $ indentLevel tr'
  let contents = concatMap render $ zip [0 ..] (concat rendered0)
      defViewPort = ViewPort (0, 0) canvasDim
      extent = L.foldl' viewPortSum defViewPort $ fmap primBoundingBox contents
  pure
    Scene
      { sceneId = "module-tree",
        sceneGlobalViewPort = extent,
        sceneLocalViewPort = extent,
        sceneElements = contents,
        sceneExtents = Just extent
      }
  where
    timing = ss._serverTiming._tsTimingMap
    drvModMap = ss._serverDriverModuleMap
    mexpandedModu = srcUI._srcViewExpandedModule
    expanded = maybe [] (T.splitOn ".") mexpandedModu
    displayedForest =
      expandFocusOnly expanded . fmap markLeaf $
        ss._serverModuleGraphState._mgsModuleForest
    displayedForest' :: [Tree (ModuleName, Bool)]
    displayedForest' =
      fmap (fmap (first (T.intercalate "."))) . fmap (accumPrefix []) $ displayedForest
    breakpoints = ss._serverModuleBreakpoints

    renderNode :: (ModuleName, Bool) -> m [Primitive SourceViewEvent]
    renderNode (modu, b) = do
      let doesModuleExist =
            modu `Set.member` ss._serverModuleGraphState._mgsModuleNames
          color
            | isModuleCompilationDone drvModMap timing modu = Green
            | otherwise = Black
          (matched, txt) =
            case mexpandedModu of
              Just modu'
                | modu == modu' -> (True, expandableText True (not b) modu)
              _ -> (False, expandableText False (not b) modu)
          hasBreakpoint = modu `elem` breakpoints
          colorBreakpoint
            | hasBreakpoint = Just Red
            | otherwise = Nothing
          colorBox
            | matched = Just Gray
            | otherwise = Just White
          hitEvent
            | matched =
                HitEvent
                  { hitEventHoverOn = Nothing,
                    hitEventHoverOff = Nothing,
                    hitEventClick = Just (Left UnselectModule)
                  }
            | otherwise =
                HitEvent
                  { hitEventHoverOn = Nothing,
                    hitEventHoverOff = Nothing,
                    hitEventClick = Just (Right (SelectModule modu))
                  }
          hitEventBreakpoint =
            HitEvent
              { hitEventHoverOn = Nothing,
                hitEventHoverOff = Nothing,
                hitEventClick = Just (Right (SetBreakpoint modu (not hasBreakpoint)))
              }
      renderedText <- drawText' (0, 0) UpperLeft Sans color 8 txt
      let bbox = primBoundingBox renderedText
          w = viewPortWidth bbox
          h = viewPortHeight bbox
      pure $
        [ rectangle (0, 0) w h (Just Red) colorBox Nothing (Just hitEvent),
          renderedText
        ]
          ++ if doesModuleExist
            then [rectangle (w + 3, 0) 10 10 (Just Black) colorBreakpoint (Just 1.0) (Just hitEventBreakpoint)]
            else []

    annotateLevel :: a -> [Tree (Int, a)] -> Tree (Int, a)
    annotateLevel x ys = Node (0, x) (fmap (fmap (\(l, txt) -> (l + 1, txt))) ys)

    indentLevel = flatten . foldTree annotateLevel

    render :: (Int, (Int, [Primitive SourceViewEvent])) -> [Primitive SourceViewEvent]
    render (i, (j, ps)) =
      let x = fromIntegral j * 20
          y = fromIntegral i * 14
       in fmap (movePrimitiveBy (x, y)) ps
