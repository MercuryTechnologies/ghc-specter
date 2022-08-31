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
import Control.Monad.Trans.State
  ( StateT (..),
    evalStateT,
    get,
    modify',
  )
import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Data.Foldable qualified as F
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Maybe (maybeToList)

-- | Global state of the BFS search holding a set of visited nodes.
newtype BFSState = BFSState
  { bfsVisited :: IntSet
  }
  deriving (Show)

-- | Staged BFS search path from a seed.
-- The nested list represents the searched nodes per each stage, for example,
-- [[1,2,3], [4,5]] means visited 1, 2 and 3 at the first stage and 4 and 5
-- at the second stage. The candidates for the next stage, bfsNextStage, are
-- not guaranteed to be included in the path yet as a search from another
-- seed may take the node earlier.
-- Therefore, note that this BFSPath type is a temporary data and only
-- bfsSearchResult :: [[Int]] is the final result.
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
  -- | Left: search in progress, as temporary BFSPath, Right: search done and result is [[Int]].
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
  (Monad m) =>
  (BFSPath -> StateT BFSState m ()) ->
  IntMap [Int] ->
  Int ->
  m [[Int]]
runStagedBFS hook graph seed = evalStateT (loopM go startPath) startState
  where
    startState = BFSState (IS.singleton seed)
    startPath = BFSPath [[seed]] [seed]
    go path = do
      e <- stepBFS graph path
      case e of
        Left p -> hook p
        _ -> pure ()
      pure e

data MultiBFSPath = MultiBFSPath
  { mbfsSearchResultDone :: [(Int, [[Int]])]
  , mbfsSearchResultInProgress :: [(Int, BFSPath)]
  }

stepMultiseedBFS ::
  (Monad m) =>
  IntMap [Int] ->
  MultiBFSPath ->
  StateT
    BFSState
    m
    (Either MultiBFSPath [(Int, [[Int]])])
stepMultiseedBFS graph (MultiBFSPath dones notDones) = do
  es :: [Either (Int, BFSPath) (Int, [[Int]])] <-
    traverse (\(seed, path) -> bimap (seed,) (seed,) <$> stepBFS graph path) notDones
  let (notDones', newDones) = partitionEithers es
      dones' = dones ++ newDones
  case notDones' of
    [] -> pure $ Right dones'
    _ -> pure $ Left (MultiBFSPath dones' notDones')

runMultiseedStagedBFS ::
  (Monad m) =>
  (MultiBFSPath -> StateT BFSState m ()) ->
  IntMap [Int] ->
  [Int] ->
  m [(Int, [[Int]])]
runMultiseedStagedBFS hook graph seeds =
  evalStateT (loopM go startMultiBFSPath) startState
  where
    startState = BFSState $ IS.fromList seeds
    seedPaths = fmap (\seed -> (seed, BFSPath [[seed]] [seed])) seeds
    startMultiBFSPath = MultiBFSPath [] seedPaths
    go mpath = do
      e <- stepMultiseedBFS graph mpath
      case e of
        Left mp -> hook mp
        _ -> pure ()
      pure e
