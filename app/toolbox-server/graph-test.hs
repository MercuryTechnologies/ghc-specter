module Main (main) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import System.IO (IOMode (ReadMode), withFile)
import Toolbox.Channel (ModuleGraphInfo (..))
import Toolbox.Util.Graph
  ( ClusterState (..),
    ClusterVertex (..),
    GraphState (..),
    degreeInvariant,
    filterOutSmallNodes,
    fullStep,
    getBiDepGraph,
    makeSeedState,
    testGraphInfo,
    totalNumberInvariant,
  )

main :: IO ()
main = do
{-  withFile "./modulegraph.dat" ReadMode $ \h -> do
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
