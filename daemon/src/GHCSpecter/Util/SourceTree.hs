module GHCSpecter.Util.SourceTree
  ( test,
  )
where

import Control.Lens (to, (^..))
import Data.Foldable qualified as F
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

test :: ServerState -> IO ()
test ss = do
  let modNames =
        L.sort $
          ss
            ^.. serverSessionInfo
              . to sessionModuleGraph
              . to mginfoModuleNameMap
              . traverse
              . to (T.splitOn ".")
      forest = L.foldl' (\(!ts) m -> appendTo m ts) [] modNames
  mapM_ print modNames
  putStrLn $
    drawForest (fmap (fmap T.unpack) forest)
