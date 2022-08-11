module Main (main) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import System.IO (IOMode (ReadMode), withFile)
import Toolbox.Channel (ModuleGraphInfo (..))
import Toolbox.Render.ModuleGraph
  ( drawGraph,
    makeReducedGraphReversedFromModuleGraph,
  )
import Toolbox.Util.Graph
  ( ClusterState (..),
    ClusterVertex (..),
    GraphState (..),
    degreeInvariant,
    diffCluster,
    fullStep,
    getBiDepGraph,
    makeSeedState,
    totalNumberInvariant,
  )

main :: IO ()
main = do
  withFile "./modulegraph.dat" ReadMode $ \h -> do
    lbs <- BL.hGetContents h
    case eitherDecode @ModuleGraphInfo lbs of
      Left e -> print e
      Right mgi -> do
        let reducedGraphReversed =
              makeReducedGraphReversedFromModuleGraph mgi
        v <- drawGraph (mginfoModuleNameMap mgi) reducedGraphReversed
        print v
