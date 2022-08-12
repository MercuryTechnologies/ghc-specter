module Main (main) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import Data.Graph (buildG, components, dff, topSort)
import qualified Data.IntMap as IM
import qualified Data.List as L
import System.IO (IOMode (ReadMode), withFile)
import Toolbox.Channel (ModuleGraphInfo (..))
import Toolbox.Render.ModuleGraph
  ( drawGraph,
    filterOutSmallNodes,
    ogdfTest,
  )
import Toolbox.Util.Graph
  ( ClusterState (..),
    ClusterVertex (..),
    GraphState (..),
    degreeInvariant,
    diffCluster,
    fullStep,
    getBiDepGraph,
    makeEdges,
    makeReducedGraph,
    makeSeedState,
    mkRevDep,
    reduceGraph,
    testGraphInfo,
    totalNumberInvariant,
  )

testPrint :: [Int] -> ModuleGraphInfo -> IO ()
testPrint largeNodes mgi = do
  let bgr = getBiDepGraph mgi
      allNodes = fmap fst $ mginfoModuleNameMap mgi
      smallNodes = allNodes L.\\ largeNodes
      seeds =
        ClusterState
          { clusterStateClustered =
              fmap (\i -> (Cluster i, [i])) largeNodes
          , clusterStateUnclustered = smallNodes
          }
  let clustering0 = seeds
      r0 = (seeds, makeSeedState largeNodes bgr)
      r1@(clustering1, graph1) = fullStep r0
      r2@(clustering2, graph2) = fullStep r1
      r3 = fullStep r2
      r4 = fullStep r3
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
        print (length (clusterStateUnclustered (fst r)))
  putStrLn "###############################"
  putStrLn "############# r0 ##############"
  putStrLn "###############################"
  printFunc r0
  putStrLn "###############################"
  putStrLn "############# r1 ##############"
  putStrLn "###############################"
  printFunc r1
  mapM_ print (diffCluster clustering1 clustering0)
  putStrLn "###############################"
  putStrLn "############# r2 ##############"
  putStrLn "###############################"
  printFunc r2
  mapM_ print (diffCluster clustering2 clustering1)
  putStrLn "###############################"
  putStrLn "############# r3 ##############"
  putStrLn "###############################"
  printFunc r3
  putStrLn "###############################"
  putStrLn "############# r4 ##############"
  putStrLn "###############################"
  printFunc r4

main' :: IO ()
main' = do
  withFile "./modulegraph.dat" ReadMode $ \h -> do
    lbs <- BL.hGetContents h
    case eitherDecode @ModuleGraphInfo lbs of
      Left e -> print e
      Right mgi -> do
        let -- mgi = testGraphInfo
            -- seeds = [2, 5, 8]
            seeds = filterOutSmallNodes mgi
        -- print $ reduceGraph seeds mgi
        ogdfTest False "test2.svg" seeds mgi
        pure ()
-- testPrint seeds mgi

main :: IO ()
main = do
  withFile "./modulegraph.dat" ReadMode $ \h -> do
    lbs <- BL.hGetContents h
    case eitherDecode @ModuleGraphInfo lbs of
      Left e -> print e
      Right mgi -> do
        let es = makeEdges $ mginfoModuleDep mgi
            g = buildG (1, 2683) es
            seeds = filterOutSmallNodes mgi
        print (length (dff g))
        print (length (components g))
        let comps = components g
            icomps = zip [1 ..] (fmap F.toList comps)
            isIn v (_i, comp) = v `elem` comp
        mapM_ print $
          fmap (\i -> fmap fst $ filter (isIn i) icomps) seeds
        let tordVtxs = topSort g
            tordSeeds = filter (`elem` seeds) tordVtxs
            reducedGraph = makeReducedGraph g tordSeeds

        let reducedGraphReversed = IM.toList $ mkRevDep reducedGraph

        v <- drawGraph "newdep.svg" (mginfoModuleNameMap mgi) reducedGraphReversed
        print v
