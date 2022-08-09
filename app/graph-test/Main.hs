{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Extra (loop)
import Control.Monad.Trans.State (execState, get, modify')
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Discrimination (inner)
import Data.Discrimination.Grouping (grouping)
import Data.Either (partitionEithers)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (First (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import System.IO (IOMode (..), withFile)
import Toolbox.Channel (ModuleGraphInfo (..))

-- | representative vertex, other vertices that belong to this cluster
newtype ClusterVertex = Cluster Int
  deriving (Show, Eq, Ord)

repCluster :: ClusterVertex -> Int
repCluster (Cluster v) = v

-- | intermediate cluster vertex
data ICVertex
  = Unclustered Int
  | Clustered ClusterVertex
  deriving (Show, Eq, Ord)

initICVertex :: Int -> ICVertex
initICVertex i = Unclustered i

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
matchCluster clustering v =
  let clustered = clusterStateClustered clustering
      match v (cls, vs)
        | v `elem` vs = First (Just cls)
        | otherwise = First Nothing
   in getFirst (foldMap (match v) clustered)

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

replaceStepA ::
  ClusterState ->
  GraphState ->
  GraphState
replaceStepA clustering (GraphState clusteredGraph unclusteredGraph) =
  GraphState (fmap replace clusteredGraph) unclusteredGraph
  where
    replaceV c@(Clustered {}) = c
    replaceV (Unclustered v) =
      case matchCluster clustering v of
        Nothing -> Unclustered v
        Just cls -> Clustered cls
    --
    replaceE _ograph (isOut, x@(Clustered {})) = [(isOut, x)]
    replaceE ograph (isOut, x@(Unclustered v)) =
      case matchCluster clustering v of
        Nothing -> [(isOut, x)]
        Just cls ->
          let (os, is) = fromMaybe ([], []) $ L.lookup v unclusteredGraph
              os' = fmap ((True,) . replaceV) os
              is' = fmap ((False,) . replaceV) is
           in [(isOut, Clustered cls)] ++ os' ++ is'
    --
    replace (v, (os, is)) =
      let (os1, is1) =
            L.partition fst $
              concatMap (replaceE clusteredGraph) $
                (fmap (True,) os ++ fmap (False,) is)
          os' = {- filter (/= Clustered v) $ -} L.nub $ L.sort $ fmap snd os1
          is' = {- filter (/= Clustered v) $ -} L.nub $ L.sort $ fmap snd is1
       in (v, (os', is'))

pruneStepB ::
  ClusterState ->
  GraphState ->
  GraphState
pruneStepB clustering (GraphState clusteredGraph unclusteredGraph) = GraphState clusteredGraph unclusteredGraph'
  where
    unclustered = clusterStateUnclustered clustering
    unclusteredGraph' = filter (\(v, _) -> v `elem` unclustered) unclusteredGraph

replaceStepC ::
  ClusterState ->
  GraphState ->
  GraphState
replaceStepC clustering (GraphState clusteredGraph unclusteredGraph) =
  (GraphState clusteredGraph (fmap replace unclusteredGraph))
  where
    replaceV c@(Clustered {}) = c
    replaceV (Unclustered v) =
      case matchCluster clustering v of
        Nothing -> Unclustered v
        Just cls -> Clustered cls
    --
    replace (v, (os, is)) =
      let os' = L.nub $ L.sort $ fmap replaceV os
          is' = L.nub $ L.sort $ fmap replaceV is
       in (v, (os', is'))

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
      let (os, is) = fromMaybe ([], []) $ L.lookup cls clusteredGraph
          os' = getUnclustered os
          is' = getUnclustered is
          added = ((os' ++ is') L.\\ vs) `L.intersect` unclustered
          vs' = L.nub $ L.sort (vs ++ added)
          unclustered' = L.nub $ L.sort (unclustered L.\\ added)
      modify' (\s -> s {clusterStateUnclustered = unclustered'})
      pure (cls, vs')

fullStep ::
  (ClusterState, GraphState) ->
  (ClusterState, GraphState)
fullStep (clustering, graphState) =
  let graphState1 = trace ("graphState: " ++ show (degreeInvariant graphState)) $ replaceStepA clustering graphState
      graphState2 = trace ("graphState1: " ++ show (degreeInvariant graphState1)) $ pruneStepB clustering graphState1
      graphState3 = trace ("graphState2: " ++ show (degreeInvariant graphState2)) $ replaceStepC clustering graphState2
      clustering' = trace ("graphState3: " ++ show (degreeInvariant graphState3)) $ clusterStep graphState3 clustering
   in clustering' `seq` graphState3 `seq` (clustering', graphState3)

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

analyze :: ModuleGraphInfo -> Text
analyze graphInfo =
  let modDep = mginfoModuleDep graphInfo
      modRevDepMap = mkRevDep modDep
      modRevDep = IM.toList modRevDepMap
      initials = fmap fst $ filter (\(_, js) -> null js) modDep
      terminals = fmap fst $ filter (\(_, js) -> null js) modRevDep
      orphans = initials `L.intersect` terminals
      singles = mapMaybe (\(i, js) -> case js of j : [] -> Just (i, j); _ -> Nothing) modDep
      leg i = loop go ([i], i)
        where
          go (acc', i') =
            case L.lookup i' singles of
              Nothing -> Right acc'
              Just j' -> Left (acc' ++ [j'], j')
      legs = fmap leg (initials L.\\ orphans)
      larges = filterOutSmallNodes graphInfo
      largeNames = mapMaybe (\i -> L.lookup i (mginfoModuleNameMap graphInfo)) larges
   in "intials: " <> (T.pack $ show initials) <> ",\n"
        <> "terminals: "
        <> (T.pack $ show terminals)
        <> ",\n"
        <> "orphans: "
        <> (T.pack $ show orphans)
        <> ",\n"
        <> "singles: "
        <> (T.pack $ show singles)
        <> ",\n"
        <> "legs: "
        <> (T.pack $ show legs)
        <> "\n=============\n"
        <> "larges: "
        <> (T.pack $ show largeNames)
        <> "\n-------------\n"
        <> "# of larges: "
        <> (T.pack $ show (length larges))

-- | (number of vertices, number of edges)
stat :: ModuleGraphInfo -> (Int, Int)
stat mgi =
  let nVtx = length $ mginfoModuleNameMap mgi
      nEdg = sum $ fmap (length . snd) $ mginfoModuleDep mgi
   in (nVtx, nEdg)

formatModuleGraphInfo :: ModuleGraphInfo -> Text
formatModuleGraphInfo mgi =
  let txt1 =
        T.intercalate "\n" . fmap (T.pack . show) $ mginfoModuleNameMap mgi
      txt2 =
        T.intercalate "\n" . fmap (T.pack . show) $ mginfoModuleDep mgi
      txt3 =
        T.pack . show $ mginfoModuleTopSorted mgi
      (nVtx, nEdg) = stat mgi
   in "(key, module):\n"
        <> txt1
        <> "\n-----------------\n"
        <> "dependencies:\n"
        <> txt2
        <> "\n-----------------\n"
        <> "top sorted:\n"
        <> txt3
        <> "\n=================\n"
        <> analyze mgi
        <> "\n=================\n"
        <> "# of vertices: "
        <> T.pack (show nVtx)
        <> ", # of edges: "
        <> T.pack (show nEdg)

testGraph =
  [ (1, [])
  , (2, [1, 4, 5, 6])
  , (3, [6])
  , (4, [])
  , (5, [4, 7, 8])
  , (6, [8])
  , (7, [9, 10])
  , (8, [9])
  , (9, [])
  , (10, [])
  ]

testGraphInfo =
  ModuleGraphInfo
    { mginfoModuleNameMap =
        [ (1, "A")
        , (2, "B")
        , (3, "C")
        , (4, "D")
        , (5, "E")
        , (6, "F")
        , (7, "G")
        , (8, "H")
        , (9, "I")
        , (10, "J")
        ]
    , mginfoModuleDep = testGraph
    , mginfoModuleTopSorted = []
    }

main :: IO ()
main = do
  {-withFile "./modulegraph.dat" ReadMode $ \h -> do
    lbs <- BL.hGetContents h
    case eitherDecode @ModuleGraphInfo lbs of
      Left e -> print e
      Right mgi -> do -}
  let mgi = testGraphInfo
  let gr = mginfoModuleDep mgi
      bgr = getBiDepGraph mgi
      allNodes = fmap fst $ mginfoModuleNameMap mgi
      largeNodes = [2, 5, 8] -- filterOutSmallNodes mgi
      smallNodes = allNodes L.\\ largeNodes
      seeds =
        ClusterState
          { clusterStateClustered =
              fmap (\i -> (Cluster i, [i])) largeNodes
          , clusterStateUnclustered = smallNodes
          }
  let r0 = (seeds, makeSeedState largeNodes bgr)
      r1@(clustering1, graph1) = fullStep r0
      r2@(clustering2, graph2) = fullStep r1
      r3 = fullStep r2
      printFunc r = do
        mapM_ (\x -> print x >> putStrLn "") (graphStateClustered $ snd r)
        mapM_ (\x -> print x >> putStrLn "") (graphStateUnclustered $ snd r)
        putStrLn "---------"
        mapM_ print (clusterStateClustered (fst r))
        putStrLn "---------"
        print (clusterStateUnclustered (fst r))
        putStrLn "---------"

        print (totalNumberInvariant (fst r))
        print (degreeInvariant (snd r))
  putStrLn "###############################"
  putStrLn "############# r0 ##############"
  putStrLn "###############################"
  printFunc r0

  putStrLn "###############################"
  putStrLn "############# r1 ##############"
  putStrLn "###############################"
  printFunc r1
  putStrLn "###############################"
  putStrLn "############# r2 ##############"
  putStrLn "###############################"
  printFunc r2
  putStrLn "###############################"
  putStrLn "############# r3 ##############"
  putStrLn "###############################"
  printFunc r3

-- -}
