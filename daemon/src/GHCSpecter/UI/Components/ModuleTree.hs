{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.UI.Components.ModuleTree (
  buildModuleTree,
  expandableText,
) where

import Control.Lens (to, (^.))
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..), flatten, foldTree)
import GHCSpecter.Channel.Common.Types (type ModuleName)
import GHCSpecter.Data.Timing.Util (isModuleCompilationDone)
import GHCSpecter.Graphics.DSL (
  Color (..),
  HitEvent (..),
  Primitive (..),
  Scene (..),
  TextFontFace (..),
  TextPosition (..),
  ViewPort (..),
  drawText,
  rectangle,
 )
import GHCSpecter.Layouter.Box.Flow (movePrimitiveBy)
import GHCSpecter.Server.Types (
  HasModuleGraphState (..),
  HasServerState (..),
  HasTimingState (..),
  ServerState (..),
 )
import GHCSpecter.UI.Constants (canvasDim)
import GHCSpecter.UI.Types (
  HasSourceViewUI (..),
  SourceViewUI (..),
 )
import GHCSpecter.UI.Types.Event (SourceViewEvent (..))
import GHCSpecter.Util.SourceTree (
  accumPrefix,
  expandFocusOnly,
  markLeaf,
 )

expandableText :: Bool -> Bool -> Text -> Text
expandableText isBordered isExpandable txt =
  let txt'
        | not isBordered && isExpandable = txt <> " ... "
        | otherwise = txt
   in txt'

buildModuleTree :: SourceViewUI -> ServerState -> Scene (Primitive SourceViewEvent)
buildModuleTree srcUI ss =
  Scene
    { sceneId = "module-tree"
    , sceneGlobalViewPort = ViewPort (0, 0) canvasDim
    , sceneLocalViewPort = ViewPort (0, 0) canvasDim
    , sceneElements = contents
    , sceneExtent = Nothing
    }
  where
    timing = ss ^. serverTiming . tsTimingMap
    drvModMap = ss ^. serverDriverModuleMap
    mexpandedModu = srcUI ^. srcViewExpandedModule
    expanded = maybe [] (T.splitOn ".") mexpandedModu
    displayedForest =
      ss ^. serverModuleGraphState . mgsModuleForest . to (expandFocusOnly expanded . fmap markLeaf)
    displayedForest' :: [Tree (ModuleName, Bool)]
    displayedForest' =
      fmap (fmap (first (T.intercalate "."))) . fmap (accumPrefix []) $ displayedForest
    breakpoints = ss ^. serverModuleBreakpoints

    renderNode :: (ModuleName, Bool) -> [Primitive SourceViewEvent]
    renderNode (modu, b) =
      let color
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
                  { hitEventHoverOn = Nothing
                  , hitEventHoverOff = Nothing
                  , hitEventClick = Just (Left UnselectModule)
                  }
            | otherwise =
                HitEvent
                  { hitEventHoverOn = Nothing
                  , hitEventHoverOff = Nothing
                  , hitEventClick = Just (Right (SelectModule modu))
                  }
          hitEventBreakpoint =
            HitEvent
              { hitEventHoverOn = Nothing
              , hitEventHoverOff = Nothing
              , hitEventClick = Just (Right (SetBreakpoint modu (not hasBreakpoint)))
              }
       in [ rectangle (0, 0) 100 10 Nothing colorBox Nothing (Just hitEvent)
          , drawText (0, 0) UpperLeft Sans color 8 txt
          , rectangle (150, 0) 10 10 (Just Black) colorBreakpoint (Just 1.0) (Just hitEventBreakpoint)
          ]

    annotateLevel :: a -> [Tree (Int, a)] -> Tree (Int, a)
    annotateLevel x ys = Node (0, x) (fmap (fmap (\(l, txt) -> (l + 1, txt))) ys)

    indentLevel = flatten . foldTree annotateLevel . fmap renderNode

    render :: (Int, (Int, [Primitive SourceViewEvent])) -> [Primitive SourceViewEvent]
    render (i, (j, ps)) =
      let x = fromIntegral j * 20
          y = fromIntegral i * 14
       in fmap (movePrimitiveBy (x, y)) ps

    contents = concatMap render $ zip [0 ..] (concatMap indentLevel displayedForest')
