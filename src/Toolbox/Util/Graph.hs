{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Toolbox.Util.Graph
  ( ClusterState (..),
    ClusterVertex (..),
    GraphState (..),
    ICVertex (..),
    annotateLevel,
    degreeInvariant,
    diffCluster,
    filterOutSmallNodes,
    getBiDepGraph,
    fullStep,
    makeEdges,
    makeReducedGraph,
    makeReducedGraphReversedFromModuleGraph,
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
import qualified Data.Foldable as F
import Data.Graph (Graph, Tree (..), buildG, path, topSort)
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

mkRevDep :: IntMap [Int] -> IntMap [Int]
mkRevDep deps = IM.foldlWithKey step emptyMap deps
  where
    emptyMap = fmap (const []) deps
    step !acc i js =
      L.foldl' (\(!acc') j -> IM.insertWith (<>) j [i] acc') acc js

-- TODO: Place this to some common module for constants
nodeSizeLimit :: Int
nodeSizeLimit = 150

-- | graph to edge list
makeEdges :: IntMap [Int] -> [(Int, Int)]
makeEdges = concatMap (\(i, js) -> fmap (i,) js) . IM.toList

-- | tree level annotation
annotateLevel :: Int -> Tree a -> Tree (Int, a)
annotateLevel root (Node x ys) = Node (root, x) (fmap (annotateLevel (root + 1)) ys)

-- | strip down graph to a given topologically ordered subset
makeReducedGraph :: Graph -> [Int] -> IntMap [Int]
makeReducedGraph g tordList = IM.fromList $ go tordList
  where
    go ys =
      case ys of
        [] -> []
        x : [] -> [(x, [])]
        x : xs ->
          (x, concatMap (\y -> if path g x y then [y] else []) xs) : go xs

-- (dep, revdep) per each vertex
getBiDepGraph :: ModuleGraphInfo -> IntMap ([Int], [Int])
getBiDepGraph graphInfo =
  let modDep = mginfoModuleDep graphInfo
      modRevDep = mkRevDep modDep
      -- NOTE: The @inner@ join function has O(n) complexity using radix sort.
      modBiDep = concat $ inner grouping joiner fst fst (IM.toList modDep) (IM.toList modRevDep)
        where
          joiner (i, js) (_, ks) = (i, (js, ks))
   in IM.fromList modBiDep

filterOutSmallNodes :: ModuleGraphInfo -> [Int]
filterOutSmallNodes graphInfo =
  let modBiDep = getBiDepGraph graphInfo
   in IM.keys $
        IM.filter
          (\(js, ks) -> length js + length ks > nodeSizeLimit)
          modBiDep

reduceGraph :: Bool -> [Int] -> ModuleGraphInfo -> IntMap [Int]
reduceGraph onlyClustered seeds graphInfo =
  let bgr = getBiDepGraph graphInfo
      allNodes = IM.keys $ mginfoModuleNameMap graphInfo
      smallNodes = allNodes L.\\ seeds
      seedClustering =
        ClusterState
          { clusterStateClustered =
              fmap (\i -> (Cluster i, [i])) seeds
          , clusterStateUnclustered = smallNodes
          }
      final = fullStep $ fullStep (seedClustering, makeSeedState seeds bgr)
      strip (Clustered (Cluster c)) = c
      strip (Unclustered i) = i
      finalGraphClustered = fmap (\(Cluster c, (os, _is)) -> (c, fmap strip os)) $ graphStateClustered $ snd final
      finalGraphUnclustered = fmap (\(v, (os, _is)) -> (v, fmap strip os)) $ graphStateUnclustered $ snd final
      finalGraph
        | onlyClustered = finalGraphClustered
        | otherwise = finalGraphClustered ++ finalGraphUnclustered
   in IM.fromList finalGraph

makeReducedGraphReversedFromModuleGraph :: ModuleGraphInfo -> IntMap [Int]
makeReducedGraphReversedFromModuleGraph mgi =
  let nVtx = F.length $ mginfoModuleNameMap mgi
      es = makeEdges $ mginfoModuleDep mgi
      g = buildG (1, nVtx) es
      seeds = filterOutSmallNodes mgi
      tordVtxs = topSort g
      tordSeeds = filter (`elem` seeds) tordVtxs
      reducedGraph = makeReducedGraph g tordSeeds
   in mkRevDep reducedGraph
