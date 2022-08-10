{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Toolbox.Util.Graph
  ( ClusterState (..),
    ClusterVertex (..),
    GraphState (..),
    ICVertex (..),
    degreeInvariant,
    diffCluster,
    filterOutSmallNodes,
    getBiDepGraph,
    fullStep,
    makeSeedState,
    mkRevDep,
    reduceGraph,
    totalNumberInvariant,
  )
where

import Control.Monad.Trans.State (execState, get, modify')
import Data.Discrimination (inner)
import Data.Discrimination.Grouping (grouping)
import Data.Either (partitionEithers)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Monoid (First (..))
import Toolbox.Channel (ModuleGraphInfo (..))

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

makeSeedState ::
  [Int] ->
  [(Int, ([Int], [Int]))] ->
  GraphState
makeSeedState seeds graph = GraphState clusteredGraph unclusteredGraph
  where
    convert (v, (os, is)) =
      let os' = fmap Unclustered $ L.nub $ L.sort os
          is' = fmap Unclustered $ L.nub $ L.sort is
       in if v `elem` seeds
            then Left (Cluster v, (os', is'))
            else Right (v, (os', is'))
    (clusteredGraph, unclusteredGraph) = partitionEithers $ fmap convert graph

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

mkRevDep :: [(Int, [Int])] -> IntMap [Int]
mkRevDep deps = L.foldl' step emptyMap deps
  where
    emptyMap = L.foldl' (\(!acc) (i, _) -> IM.insert i [] acc) IM.empty deps
    step !acc (i, js) =
      L.foldl' (\(!acc') j -> IM.insertWith (<>) j [i] acc') acc js

nodeSizeLimit :: Int
nodeSizeLimit = 150

-- ( vertex, (dep, revdep)) per each item
getBiDepGraph :: ModuleGraphInfo -> [(Int, ([Int], [Int]))]
getBiDepGraph graphInfo =
  let modDep = mginfoModuleDep graphInfo
      modRevDepMap = mkRevDep modDep
      modRevDep = IM.toList modRevDepMap
      modBiDep = concat $ inner grouping joiner fst fst modDep modRevDep
        where
          joiner (i, js) (_, ks) = (i, (js, ks))
   in modBiDep

filterOutSmallNodes :: ModuleGraphInfo -> [Int]
filterOutSmallNodes graphInfo =
  let modBiDep = getBiDepGraph graphInfo
   in fmap fst $
        filter
          (\(_, (js, ks)) -> length js + length ks > nodeSizeLimit)
          modBiDep

reduceGraph :: Bool -> [Int] -> ModuleGraphInfo -> [(Int, [Int])]
reduceGraph onlyClustered seeds graphInfo =
  let bgr = getBiDepGraph graphInfo
      allNodes = fmap fst $ mginfoModuleNameMap graphInfo
      smallNodes = allNodes L.\\ seeds
      seedClustering =
        ClusterState
          { clusterStateClustered =
              fmap (\i -> (Cluster i, [i])) seeds
          , clusterStateUnclustered = smallNodes
          }
      r0 = (seedClustering, makeSeedState seeds bgr)
      -- r1@(clustering1, graph1) = fullStep r0
      -- r2@(clustering2, graph2) = fullStep r1
      -- r3 = fullStep r2
      -- r4 = fullStep r3
      final = r0
      strip (Clustered (Cluster c)) = c
      strip (Unclustered i) = i
      finalGraphClustered = fmap (\(Cluster c, (os, _is)) -> (c, fmap strip os)) $ graphStateClustered $ snd final
      finalGraphUnclustered = fmap (\(v, (os, _is)) -> (v, fmap strip os)) $ graphStateUnclustered $ snd final
      finalGraph
        | onlyClustered = finalGraphClustered
        | otherwise = finalGraphClustered ++ finalGraphUnclustered
   in finalGraph
