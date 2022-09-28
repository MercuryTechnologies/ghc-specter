module GHCSpecter.Util.SourceTree
  ( makeSourceTree,
    accumPrefix,
    expandFocusOnly,
  )
where

import Control.Lens (to, (^..))
import Data.List qualified as L
import Data.Text qualified as T
import Data.Tree (Forest, Tree (..))
import GHCSpecter.Channel.Common.Types (type ModuleName)
import GHCSpecter.Channel.Outbound.Types (ModuleGraphInfo (..))

appendTo :: [ModuleName] -> Forest ModuleName -> Forest ModuleName
appendTo [] ts = ts
appendTo (x : xs) [] = [Node x (appendTo xs [])]
appendTo (x : xs) (t : ts)
  | x == rootLabel t = Node x (appendTo xs (subForest t)) : ts
  | otherwise = t : appendTo (x : xs) ts

makeSourceTree :: ModuleGraphInfo -> Forest ModuleName
makeSourceTree mgi =
  let modNames =
        L.sort $
          mginfoModuleNameMap mgi
            ^.. traverse
              . to (T.splitOn ".")
   in L.foldl' (\(!ts) m -> appendTo m ts) [] modNames

stripSubTree :: Tree a -> Tree a
stripSubTree (Node x _) = Node x []

accumPrefix :: [a] -> Tree a -> Tree [a]
accumPrefix prefix (Node x xs) =
  let prefix' = prefix ++ [x]
   in Node prefix' (fmap (accumPrefix prefix') xs)

expandFocusOnly :: [ModuleName] -> Forest ModuleName -> Forest ModuleName
expandFocusOnly [] ts = fmap stripSubTree ts
expandFocusOnly _ [] = []
expandFocusOnly (x : xs) (t : ts)
  | x == rootLabel t = Node x (expandFocusOnly xs (subForest t)) : fmap stripSubTree ts
  | otherwise = Node (rootLabel t) [] : expandFocusOnly (x : xs) ts
