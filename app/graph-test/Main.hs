{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Extra (loop)
import Control.Monad.Trans.State (execState, get, modify')
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Discrimination (inner)
import Data.Discrimination.Grouping (grouping)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (First (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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

clusterStep ::
  [(ICVertex, ([ICVertex], [ICVertex]))] ->
  ClusterState ->
  ClusterState
clusterStep graph clustering = flip execState clustering $ do
  clustered' <- traverse go clustered
  modify' (\s -> s {clusterStateClustered = clustered'})
  where
    clustered = clusterStateClustered clustering
    go (cls, vs) = do
      unclustered <- clusterStateUnclustered <$> get
      let (os, is) = fromMaybe ([], []) $ L.lookup (Clustered cls) graph
          os' = getUnclustered os
          is' = getUnclustered is
          added = ((os' ++ is') L.\\ vs) `L.intersect` unclustered
          vs' = L.nub $ L.sort (vs ++ added)
          unclustered' = L.nub $ L.sort (unclustered L.\\ added)
      modify' (\s -> s {clusterStateUnclustered = unclustered'})
      pure (cls, vs')

pruneStep ::
  ClusterState ->
  [(ICVertex, ([ICVertex], [ICVertex]))] ->
  [(ICVertex, ([ICVertex], [ICVertex]))]
pruneStep clustering graph = filter prune graph
  where
    unclustered = clusterStateUnclustered clustering
    prune (Clustered {}, _) = True
    prune (Unclustered v, _) = v `elem` unclustered

replaceStep ::
  ClusterState ->
  [(ICVertex, ([ICVertex], [ICVertex]))] ->
  [(ICVertex, ([ICVertex], [ICVertex]))]
replaceStep clustering graph = fmap replace graph
  where
    replaceEach :: ClusterState -> ICVertex -> ICVertex
    replaceEach clustering c@(Clustered {}) = c
    replaceEach clustering (Unclustered v) =
      let clustered = clusterStateClustered clustering
          match v (cls, vs)
            | v `elem` vs = First (Just cls)
            | otherwise = First Nothing
       in case getFirst (foldMap (match v) clustered) of
            Nothing -> Unclustered v
            (Just cls) -> Clustered cls
    replace (v, (os, is)) =
      let os' = L.nub $ L.sort $ fmap (replaceEach clustering) os
          is' = L.nub $ L.sort $ fmap (replaceEach clustering) is
          v' = replaceEach clustering v
       in (v', (os', is'))

fullStep :: (ClusterState, [(ICVertex, ([ICVertex], [ICVertex]))]) -> (ClusterState, [(ICVertex, ([ICVertex], [ICVertex]))])
fullStep (clustering, graph) = (clustering', graph'')
  where
    graph' = replaceStep clustering graph
    clustering' = clusterStep graph' clustering
    graph'' = pruneStep clustering' graph'

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

main :: IO ()
main =
  withFile "./modulegraph.dat" ReadMode $ \h -> do
    lbs <- BL.hGetContents h
    case eitherDecode @ModuleGraphInfo lbs of
      Left e -> print e
      Right mgi -> do
        let gr = mginfoModuleDep mgi
            bgr =
              fmap (\(v, (os, is)) -> (initICVertex v, (fmap initICVertex os, fmap initICVertex is))) $
                getBiDepGraph mgi
            seeds =
              let allNodes = fmap fst $ mginfoModuleNameMap mgi
                  largeNodes = filterOutSmallNodes mgi
                  smallNodes = allNodes L.\\ largeNodes
               in ClusterState
                    { clusterStateClustered =
                        fmap (\i -> (Cluster i, [i])) largeNodes
                    , clusterStateUnclustered = smallNodes
                    }
            r0 = (seeds, bgr)
            r1 = fullStep r0
            r2 = fullStep r1
            r3 = fullStep r2
            r4 = fullStep r3

        mapM_ print gr
        putStrLn "=============="
        let getRemaining = length . clusterStateUnclustered . fst
        print (getRemaining r0)
        print (getRemaining r1)
        print (getRemaining r2)
        print (getRemaining r3)
        print (getRemaining r4)

        mapM_ print (snd r4)
        putStrLn "======="
        mapM_ print (clusterStateClustered (fst r4))
        print (clusterStateUnclustered (fst r4))
