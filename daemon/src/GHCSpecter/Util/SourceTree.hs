module GHCSpecter.Util.SourceTree
  ( test,
  )
where

import Control.Lens (to, (^..))
import Data.List qualified as L
import Data.Text qualified as T
import Data.Tree
  ( Forest,
    Tree (..),
    drawForest,
  )
import GHCSpecter.Channel
  ( ModuleGraphInfo (..),
    SessionInfo (..),
    type ModuleName,
  )
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState,
  )

appendTo :: [ModuleName] -> Forest ModuleName -> Forest ModuleName
appendTo [] ts = ts
appendTo (x : xs) [] = [Node x (appendTo xs [])]
appendTo (x : xs) (t : ts)
  | x == rootLabel t = Node x (appendTo xs (subForest t)) : ts
  | otherwise = t : appendTo (x : xs) ts

makeSourceTree :: ServerState -> Forest ModuleName
makeSourceTree ss =
  let modNames =
        L.sort $
          ss
            ^.. serverSessionInfo
              . to sessionModuleGraph
              . to mginfoModuleNameMap
              . traverse
              . to (T.splitOn ".")
   in L.foldl' (\(!ts) m -> appendTo m ts) [] modNames

stripSubTree :: Tree a -> Tree a
stripSubTree (Node x _) = Node x []

expandFocusOnly :: [ModuleName] -> Forest ModuleName -> Forest ModuleName
expandFocusOnly [] ts = fmap stripSubTree ts
expandFocusOnly _ [] = []
expandFocusOnly (x : xs) (t : ts)
  | x == rootLabel t = Node x (expandFocusOnly xs (subForest t)) : fmap stripSubTree ts
  | otherwise = Node (rootLabel t) [] : expandFocusOnly (x : xs) ts

test :: ServerState -> IO ()
test ss = do
  let forest = makeSourceTree ss
      printForest f =
        putStrLn $
          drawForest (fmap (fmap T.unpack) f)
  printForest (expandFocusOnly ["Client", "Bank"] forest)
