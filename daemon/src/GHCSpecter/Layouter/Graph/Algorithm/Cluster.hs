{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module GHCSpecter.Layouter.Graph.Algorithm.Cluster (
  ClusterState (..),
  ClusterVertex (..),
  GraphState (..),
  ICVertex (..),
  annotateLevel,

  -- * invariant checks
  degreeInvariant,
  totalNumberInvariant,

  -- * reduction without clustering
  reduceGraphByPath,

  -- * reduction with clustering
  diffCluster,
  filterOutSmallNodes,
  fullStep,
  makeSeedState,
  makeDivisionsInOrder,
) where

import Control.Monad.Trans.State (execState, get, modify')
import Data.Discrimination (inner)
import Data.Discrimination.Grouping (grouping)
import Data.Either (partitionEithers)
import Data.Graph (Graph, Tree (..), path)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Monoid (First (..))
import GHCSpecter.Layouter.Graph.Algorithm.Builder (makeBiDep)

-- | representative vertex, other vertices that belong to this cluster
newtype ClusterVertex = Cluster {unCluster :: Int}
  deriving (Show, Eq, Ord)

-- | intermediate cluster vertex
data ICVertex
  = Unclustered Int
  | Clustered ClusterVertex
  deriving (Show, Eq, Ord)

getUnclustered :: [ICVertex] -> [Int]
getUnclustered =
  mapMaybe (\case Unclustered i -> Just i; _ -> Nothing)

-- cluster and elements
data ClusterState = ClusterState
  { clusterStateClustered :: [(ClusterVertex, [Int])]
  , clusterStateUnclustered :: [Int]
  }
  deriving (Show)

data GraphState = GraphState
  { graphStateClustered :: [(ClusterVertex, ([ICVertex], [ICVertex]))]
  , graphStateUnclustered :: [(Int, ([ICVertex], [ICVertex]))]
  }
  deriving (Show)

matchCluster :: ClusterState -> Int -> Maybe ClusterVertex
matchCluster clustering vtx =
  let clustered = clusterStateClustered clustering
      match v (cls, vs)
        | v `elem` vs = First (Just cls)
        | otherwise = First Nothing
   in getFirst (foldMap (match vtx) clustered)

diffCluster :: ClusterState -> ClusterState -> [(ClusterVertex, [Int])]
diffCluster c1 c2 =
  let c1cluster = clusterStateClustered c1
      c2cluster = clusterStateClustered c2
      joiner (i, js) (_, ks) = (i, js L.\\ ks)
   in concat $ inner grouping joiner (unCluster . fst) (unCluster . fst) c1cluster c2cluster

degreeInvariant :: GraphState -> (Int, Int)
degreeInvariant graphState =
  (sumRelDeg (graphStateClustered graphState), sumRelDeg (graphStateUnclustered graphState))
  where
    reldeg (_, (os, is)) = length os - length is
    sumRelDeg = sum . fmap reldeg

totalNumberInvariant :: ClusterState -> Int
totalNumberInvariant clustering =
  let clustered = clusterStateClustered clustering
      unclustered = clusterStateUnclustered clustering
   in sum (fmap (length . snd) clustered) + length unclustered

-- | Strip down graph to a given topologically ordered subset
--   with edges by path-connectedness
reduceGraphByPath :: Graph -> [Int] -> IntMap [Int]
reduceGraphByPath g tordList = IM.fromList $ go tordList
  where
    go ys =
      case ys of
        [] -> []
        x : [] -> [(x, [])]
        x : xs ->
          (x, concatMap (\y -> if path g x y then [y] else []) xs) : go xs

makeSeedState ::
  [Int] ->
  IntMap ([Int], [Int]) ->
  GraphState
makeSeedState seeds graph = GraphState clusteredGraph unclusteredGraph
  where
    convert (v, (os, is)) =
      let os' = fmap Unclustered $ L.nub $ L.sort os
          is' = fmap Unclustered $ L.nub $ L.sort is
       in if v `elem` seeds
            then Left (Cluster v, (os', is'))
            else Right (v, (os', is'))
    (clusteredGraph, unclusteredGraph) = partitionEithers $ fmap convert $ IM.toList graph

mergeStep ::
  ClusterState ->
  GraphState ->
  GraphState
mergeStep clustering (GraphState clusteredGraph unclusteredGraph) = GraphState clusteredGraph' unclusteredGraph'
  where
    merge (c, (os, is)) =
      let newdeps = do
            members <- maybeToList $ L.lookup c (clusterStateClustered clustering)
            member <- members
            maybeToList $ L.lookup member unclusteredGraph
          os' = L.nub $ L.sort $ (os ++ concatMap fst newdeps)
          is' = L.nub $ L.sort $ (is ++ concatMap snd newdeps)
       in (c, (os', is'))
    clusteredGraph' = fmap merge clusteredGraph
    unclustered = clusterStateUnclustered clustering
    unclusteredGraph' = filter (\(v, _) -> v `elem` unclustered) unclusteredGraph

replaceStep ::
  ClusterState ->
  GraphState ->
  GraphState
replaceStep clustering (GraphState clusteredGraph unclusteredGraph) = GraphState clusteredGraph' unclusteredGraph'
  where
    replaceV c@(Clustered {}) = c
    replaceV (Unclustered v) =
      case matchCluster clustering v of
        Nothing -> Unclustered v
        Just cls -> Clustered cls
    --
    replace :: (a -> ICVertex -> Bool) -> (a, ([ICVertex], [ICVertex])) -> (a, ([ICVertex], [ICVertex]))
    replace eq (v, (os, is)) =
      let os' = filter (not . eq v) $ L.nub $ L.sort $ fmap replaceV os
          is' = filter (not . eq v) $ L.nub $ L.sort $ fmap replaceV is
       in (v, (os', is'))
    --
    clusteredGraph' = fmap (replace ((==) . Clustered)) clusteredGraph
    unclusteredGraph' = fmap (replace (\_ _ -> False)) unclusteredGraph

clusterStep ::
  GraphState ->
  ClusterState ->
  ClusterState
clusterStep (GraphState clusteredGraph _) clustering = flip execState clustering $ do
  clustered' <- traverse go clustered
  modify' (\s -> s {clusterStateClustered = clustered'})
  where
    clustered = clusterStateClustered clustering
    go (cls, vs) = do
      unclustered <- clusterStateUnclustered <$> get
      let (_os, is) = fromMaybe ([], []) $ L.lookup cls clusteredGraph
          -- NOTE: only cluster towards upper dependency.
          is' = getUnclustered is
          added = (is' L.\\ vs) `L.intersect` unclustered
          vs' = L.nub $ L.sort (vs ++ added)
          unclustered' = L.nub $ L.sort (unclustered L.\\ added)
      modify' (\s -> s {clusterStateUnclustered = unclustered'})
      pure (cls, vs')

fullStep ::
  (ClusterState, GraphState) ->
  (ClusterState, GraphState)
fullStep (clustering, graphState) =
  let graphState1 = mergeStep clustering graphState
      graphState2 = replaceStep clustering graphState1
      clustering' = clusterStep graphState2 clustering
   in clustering' `seq` graphState2 `seq` (clustering', graphState2)

-- | tree level annotation
annotateLevel :: Int -> Tree a -> Tree (Int, a)
annotateLevel root (Node x ys) = Node (root, x) (fmap (annotateLevel (root + 1)) ys)

filterOutSmallNodes :: Int -> IntMap [Int] -> [Int]
filterOutSmallNodes nodeSizeLimit graph =
  IM.keys $
    IM.filter
      (\(js, ks) -> length js + length ks > nodeSizeLimit)
      (makeBiDep graph)

-- | Make division per seed, which contains vertices that do not pass the previous
--   and the next seeds in a given order.
makeDivisionsInOrder ::
  -- | sorted vertices
  [Int] ->
  -- | seed in order
  [Int] ->
  [(Int, [Int])]
makeDivisionsInOrder sorted seedsInOrder =
  let seedsInOrder' = (Nothing : fmap Just seedsInOrder) ++ [Nothing]
      iranges = zip seedsInOrder (zip seedsInOrder' (tail (tail seedsInOrder')))
      filterByRange :: (Maybe Int, Maybe Int) -> [Int] -> [Int]
      filterByRange (Nothing, Nothing) xs = xs
      filterByRange (Nothing, Just z) xs = fst $ break (== z) xs
      filterByRange (Just y, Nothing) xs = tail $ snd $ break (== y) xs
      filterByRange (Just y, Just z) xs =
        filterByRange (Nothing, Just z) . filterByRange (Just y, Nothing) $ xs
   in fmap (\(i, range) -> (i, filterByRange range sorted)) iranges
