module GHCSpecter.GraphLayout.Algorithm.Builder (
  makeEdges,
  makeRevDep,
  makeBiDep,
) where

import Data.Discrimination (inner)
import Data.Discrimination.Grouping (grouping)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List qualified as L

-- | graph to edge list
makeEdges :: IntMap [Int] -> [(Int, Int)]
makeEdges = concatMap (\(i, js) -> fmap (i,) js) . IM.toList

-- | reverse dependency graph
makeRevDep :: IntMap [Int] -> IntMap [Int]
makeRevDep deps = IM.foldlWithKey step emptyMap deps
  where
    emptyMap = fmap (const []) deps
    step !acc i js =
      L.foldl' (\(!acc') j -> IM.insertWith (<>) j [i] acc') acc js

-- | bi-dependency graph: (dep, revdep) per each vertex
makeBiDep :: IntMap [Int] -> IntMap ([Int], [Int])
makeBiDep dep =
  let revDep = makeRevDep dep
      -- NOTE: The @inner@ join function has O(n) complexity using radix sort.
      biDep = concat $ inner grouping joiner fst fst (IM.toList dep) (IM.toList revDep)
        where
          joiner (i, js) (_, ks) = (i, (js, ks))
   in IM.fromList biDep
