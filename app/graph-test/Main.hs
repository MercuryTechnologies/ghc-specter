{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad.Extra (loop)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Discrimination (inner)
import Data.Discrimination.Grouping (grouping)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Monoid (First (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (IOMode (..), withFile)
import Toolbox.Channel (ModuleGraphInfo (..))

-- | representative vertex, other vertices that belong to this cluster
newtype ClusterVertex = Cluster Int --  [Int]
  deriving (Show)

repCluster :: ClusterVertex -> Int
repCluster (Cluster v) = v

-- | intermediate cluster vertex
data ICVertex
  = Unclustered Int
  | Clustered ClusterVertex
  deriving (Show)

initICVertex :: Int -> ICVertex
initICVertex i = Unclustered i

-- cluster and elements
type ClusterState = [(ClusterVertex, [Int])]

replaceStep ::
  ClusterState ->
  [(ICVertex, ([ICVertex], [ICVertex]))] ->
  [(ICVertex, ([ICVertex], [ICVertex]))]
replaceStep clustering gr = fmap replace gr
  where
    replaceEach :: ClusterState -> ICVertex -> ICVertex
    replaceEach clustering c@(Clustered {}) = c
    replaceEach clustering (Unclustered v) =
      let match v (cls, vs)
            | v `elem` vs = First (Just cls)
            | otherwise = First Nothing
       in case getFirst (foldMap (match v) clustering) of
            Nothing -> Unclustered v
            (Just cls) -> Clustered cls
    replace (v, (os, is)) =
      let os' = fmap (replaceEach clustering) os
          is' = fmap (replaceEach clustering) is
          v' = replaceEach clustering v
       in (v', (os', is'))

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
              fmap (\i -> (Cluster i, [i])) $ filterOutSmallNodes mgi
            clustered = replaceStep seeds bgr
        print seeds
        print clustered
        --
        print (fmap fst clustered)
