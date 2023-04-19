{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Render.Components.ModuleTree (
  compileModuleTree,
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
 )
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

compileModuleTree :: SourceViewUI -> ServerState -> Scene Text
compileModuleTree srcUI ss =
  Scene
    { sceneId = "module-tree"
    , sceneGlobalViewPort = ViewPort (0, 0) canvasDim
    , sceneLocalViewPort = ViewPort (0, 0) canvasDim
    , sceneElements = contents
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
    -- breakpoints = ss ^. serverModuleBreakpoints

    renderNode :: (ModuleName, Bool) -> (Bool, Color, ModuleName, Text)
    renderNode (modu, b) =
      let color
            | isModuleCompilationDone drvModMap timing modu = Green
            | otherwise = Black
       in case mexpandedModu of
            Just modu'
              | modu == modu' -> (True, color, modu, expandableText True (not b) modu)
            _ -> (False, color, modu, expandableText False (not b) modu)

    annotateLevel :: a -> [Tree (Int, a)] -> Tree (Int, a)
    annotateLevel x ys = Node (0, x) (fmap (fmap (\(l, txt) -> (l + 1, txt))) ys)

    indentLevel = flatten . foldTree annotateLevel . fmap renderNode

    render :: (Int, (Int, (Bool, Color, ModuleName, Text))) -> [Primitive Text]
    render (i, (j, (matched, color, modu, txt))) =
      let x = fromIntegral j * 20
          y = fromIntegral i * 14
          colorBox
            | matched = Just Gray
            | otherwise = Just White
          hitEvent
            | matched =
                HitEvent
                  { hitEventHover = Nothing
                  , hitEventClick = (True, Just modu)
                  }
            | otherwise =
                HitEvent
                  { hitEventHover = Nothing
                  , hitEventClick = (False, Just modu)
                  }
       in [ Rectangle (x, y) 100 10 Nothing colorBox Nothing (Just hitEvent)
          , DrawText (x, y) UpperLeft Sans color 8 txt
          ]
    contents = concatMap render $ zip [0 ..] (concatMap indentLevel displayedForest')
