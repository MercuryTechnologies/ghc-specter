{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Util.SourceTree
  ( makeSourceTree,
    markLeaf,
    accumPrefix,
    expandFocusOnly,
  )
where

import Data.Foldable qualified as F
import Data.List qualified as L
import Data.Text qualified as T
import Data.Tree (Forest, Tree (..), foldTree)
import GHCSpecter.Channel.Common.Types (ModuleName)
import GHCSpecter.Channel.Outbound.Types (ModuleGraphInfo (..))

appendTo :: [ModuleName] -> Forest ModuleName -> Forest ModuleName
appendTo [] ts = ts
appendTo (x : xs) [] = [Node x (appendTo xs [])]
appendTo (x : xs) (t : ts)
  | x == rootLabel t = Node x (appendTo xs (subForest t)) : ts
  | otherwise = t : appendTo (x : xs) ts

-- TODO: This algorithm does not distinguish the module prefix that is not
-- matched with an actual module. We need to refine it.
makeSourceTree :: ModuleGraphInfo -> Forest ModuleName
makeSourceTree mgi =
  let modNames =
        L.sort $
          F.toList $
            fmap (T.splitOn ".") (mginfoModuleNameMap mgi)
   in L.foldl' (\(!ts) m -> appendTo m ts) [] modNames

markLeaf :: Tree a -> Tree (a, Bool)
markLeaf = foldTree go
  where
    go :: a -> [Tree (a, Bool)] -> Tree (a, Bool)
    go x ys
      | null ys = Node (x, True) []
      | otherwise = Node (x, False) ys

stripSubTree :: Tree a -> Tree a
stripSubTree (Node x _) = Node x []

accumPrefix :: [a] -> Tree (a, Bool) -> Tree ([a], Bool)
accumPrefix prefix (Node (x, b) xs) =
  let prefix' = prefix ++ [x]
   in Node (prefix', b) (fmap (accumPrefix prefix') xs)

expandFocusOnly :: [ModuleName] -> Forest (ModuleName, Bool) -> Forest (ModuleName, Bool)
expandFocusOnly [] ts = fmap stripSubTree ts
expandFocusOnly _ [] = []
expandFocusOnly (x : xs) (t : ts)
  | x == fst (rootLabel t) = Node (rootLabel t) (expandFocusOnly xs (subForest t)) : fmap stripSubTree ts
  | otherwise = Node (rootLabel t) [] : expandFocusOnly (x : xs) ts
