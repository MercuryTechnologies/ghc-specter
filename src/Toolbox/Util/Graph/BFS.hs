{-# LANGUAGE BangPatterns #-}

-- | Breadth-First-Search (BFS)
module Toolbox.Util.Graph.BFS
  ( -- * internal state for BFS
    BFSState (..),
    BFSPath (..),
    MultiBFSPath (..),

    -- * single-seed use case
    stepBFS,
    runStagedBFS,

    -- * multi-seed use case
    stepMultiseedBFS,
    runMultiseedStagedBFS,
  )
where

import Control.Monad (join)
import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
  ( StateT (..),
    evalStateT,
    get,
    modify',
  )
import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import qualified Data.Foldable as F
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Maybe (maybeToList)

newtype BFSState = BFSState
  { bfsVisited :: IntSet
  }
  deriving (Show)

data BFSPath = BFSPath
  { bfsSearchResult :: ![[Int]]
  , bfsNextStage :: ![Int]
  }
  deriving (Show)

stepBFS ::
  forall m.
  (Monad m) =>
  IntMap [Int] ->
  BFSPath ->
  StateT BFSState m (Either BFSPath [[Int]])
stepBFS graph (BFSPath searched nexts) = do
  thisStage <- F.foldlM step [] nexts
  case thisStage of
    [] -> pure $ Right searched
    _ -> do
      let searched' = searched ++ [thisStage]
      modify' (\s -> s {bfsVisited = bfsVisited s `IS.union` IS.fromList thisStage})
      pure $ Left (BFSPath searched' thisStage)
  where
    getUnvisitedChildren :: IntSet -> Int -> [Int]
    getUnvisitedChildren !visited current =
      let children :: [Int]
          children = join $ maybeToList (IM.lookup current graph)
       in filter (`IS.notMember` visited) children

    step :: [Int] -> Int -> StateT BFSState m [Int]
    step !thisStage current = do
      modify' (\s -> s {bfsVisited = bfsVisited s `IS.union` IS.fromList thisStage})
      visited <- bfsVisited <$> get
      let newChildren = getUnvisitedChildren visited current
          thisStage' = thisStage ++ newChildren
      pure thisStage'

runStagedBFS ::
  IntMap [Int] ->
  Int ->
  IO [[Int]]
runStagedBFS graph seed = evalStateT (loopM go startPath) startState
  where
    startState = BFSState (IS.singleton seed)
    startPath = BFSPath [[seed]] [seed]
    go path = do
      e <- stepBFS graph path
      case e of
        Left (BFSPath _searched staged) -> do
          visited <- bfsVisited <$> get
          liftIO $ do
            putStrLn "--------"
            print (visited, staged)
        _ -> pure ()
      pure e

data MultiBFSPath = MultiBFSPath
  { mbfsSearchResultDone :: [(Int, [[Int]])]
  , mbfsSearchResultInProgress :: [(Int, BFSPath)]
  }

stepMultiseedBFS ::
  IntMap [Int] ->
  MultiBFSPath ->
  StateT
    BFSState
    IO
    (Either MultiBFSPath [(Int, [[Int]])])
stepMultiseedBFS graph (MultiBFSPath dones notDones) = do
  es :: [Either (Int, BFSPath) (Int, [[Int]])] <-
    traverse (\(seed, path) -> bimap (seed,) (seed,) <$> stepBFS graph path) notDones
  let (notDones', newDones) = partitionEithers es
      dones' = dones ++ newDones
  case notDones' of
    [] -> pure $ Right dones'
    _ -> pure $ Left (MultiBFSPath dones' notDones')

runMultiseedStagedBFS :: IntMap [Int] -> [Int] -> IO [(Int, [[Int]])]
runMultiseedStagedBFS graph seeds =
  evalStateT (loopM (stepMultiseedBFS graph) startMultiBFSPath) startState
  where
    startState = BFSState $ IS.fromList seeds
    seedPaths = fmap (\seed -> (seed, BFSPath [[seed]] [seed])) seeds
    startMultiBFSPath = MultiBFSPath [] seedPaths
