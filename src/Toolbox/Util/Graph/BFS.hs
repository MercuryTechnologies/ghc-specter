{-# LANGUAGE BangPatterns #-}

module Toolbox.Util.Graph.BFS
  ( BFSState (..),
    stepBFS,
    runStagedBFS,
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
import Data.Bifunctor (second)
import qualified Data.Foldable as F
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.List as L
import Data.Maybe (maybeToList)

newtype BFSState = BFSState
  { bfsVisited :: IntSet
  }

emptyBFSState :: BFSState
emptyBFSState = BFSState IS.empty

data BFSPath = BFSPath
  { bfsSearchResult :: ![[Int]]
  , bfsNextStage :: ![Int]
  }

runStagedBFS :: IntMap [Int] -> Int -> IO [[Int]]
runStagedBFS graph seed = evalStateT (loopM go startPath) startState
  where
    startState = BFSState IS.empty -- BFSState (IS.singleton seed)
    startPath = BFSPath [[seed]] [seed]
    go s = do
      e <- stepBFS graph s
      case e of
        Left (BFSPath _searched staged) -> do
          visited <- bfsVisited <$> get
          liftIO $ do
            putStrLn "--------"
            print (visited, staged)
        _ -> pure ()
      pure e

stepBFS :: forall m. (Monad m) => IntMap [Int] -> BFSPath -> StateT BFSState m (Either BFSPath [[Int]])
stepBFS graph (BFSPath searched nexts) = do
  modify' (\s -> s {bfsVisited = bfsVisited s `IS.union` IS.fromList nexts})
  thisStage <- F.foldlM step [] nexts
  case thisStage of
    [] -> pure $ Right searched
    _ -> do
      let searched' = searched ++ [thisStage]
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
