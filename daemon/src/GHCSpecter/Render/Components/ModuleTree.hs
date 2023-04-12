{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Render.Components.ModuleTree (
  compileModuleTree,
  expandableText,
) where

import Control.Lens (to, (^.))
import Data.Bifunctor (first)
-- import Data.Map qualified as M
-- import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..), flatten, foldTree)
import GHCSpecter.Channel.Common.Types (type ModuleName)
import GHCSpecter.Graphics.DSL (
  Color (..),
  Primitive (..),
  Scene (..),
  TextPosition (..),
  ViewPort (..),
 )
-- import GHCSpecter.Channel.Outbound.Types (
--  Channel (..),
--  SessionInfo (..),
-- )
-- import GHCSpecter.Data.Timing.Util (isModuleCompilationDone)
import GHCSpecter.Server.Types (
  HasModuleGraphState (..),
  HasServerState (..),
  -- HasTimingState (..),
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

compileModuleTree :: SourceViewUI -> ServerState -> Scene
compileModuleTree srcUI ss =
  Scene
    { sceneId = "module-tree"
    , sceneGlobalViewPort = ViewPort (0, 0) canvasDim
    , sceneLocalViewPort = ViewPort (0, 0) canvasDim
    , sceneElements = contents
    }
  where
    -- timing = ss ^. serverTiming . tsTimingMap
    -- drvModMap = ss ^. serverDriverModuleMap
    mexpandedModu = Just "Agda.Compiler.Backend" -- srcUI ^. srcViewExpandedModule
    expanded = maybe [] (T.splitOn ".") mexpandedModu
    displayedForest =
      ss ^. serverModuleGraphState . mgsModuleForest . to (expandFocusOnly expanded . fmap markLeaf)
    displayedForest' :: [Tree (ModuleName, Bool)]
    displayedForest' =
      fmap (fmap (first (T.intercalate "."))) . fmap (accumPrefix []) $ displayedForest
    -- breakpoints = ss ^. serverModuleBreakpoints

    renderNode :: (ModuleName, Bool) -> Either (ModuleName, Text) (ModuleName, Text)
    renderNode (modu, b) =
      case mexpandedModu of
        Just modu'
          | modu == modu' -> Right (modu, expandableText True (not b) modu)
        _ -> Left (modu, expandableText False (not b) modu)

    annotateLevel :: a -> [Tree (Int, a)] -> Tree (Int, a)
    annotateLevel x ys = Node (0, x) (fmap (fmap (\(l, txt) -> (l + 1, txt))) ys)

    indentLevel = flatten . foldTree annotateLevel . fmap renderNode

    render :: (Int, (Int, (Either (ModuleName, Text) (ModuleName, Text)))) -> [Primitive]
    render (i, (j, e)) =
      let x = fromIntegral j * 20
          y = fromIntegral i * 10
       in case e of
            Left (modu, txt) ->
              -- TODO: we use ugly unsafe Select_ and Unsele_ prefix here. We will introduce
              -- proper typing mechanism later on.
              [ Rectangle (x, y) 100 10 Nothing (Just White) Nothing (Just ("Select_" <> modu))
              , DrawText (x, y) UpperLeft Black 8 txt
              ]
            Right (modu, txt) ->
              [ Rectangle (x, y) 100 10 Nothing (Just Gray) Nothing (Just ("Unsele_" <> modu))
              , DrawText (x, y) UpperLeft Black 8 txt
              ]

    contents = concatMap render $ zip [0 ..] (concatMap indentLevel displayedForest')
