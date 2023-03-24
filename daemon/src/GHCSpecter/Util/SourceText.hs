{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Util.SourceText (
  -- * Line-Column utility
  splitLineColumn,
  isContainedIn,
  sliceText,
  findText,
  addLineCol,

  -- * Top-level declaration extraction
  filterTopLevel,
  reduceDeclRange,
) where

import Control.Monad (guard)
import Control.Monad.Trans.State (State, get, put, runState)
import Data.Function (on)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T

-- TODO: newtype (Int, Int) to LineCol

-- | splitter based on line and column
-- line and col are 1-based.
splitLineColumn :: (Int, Int) -> State ((Int, Int), Text) Text
splitLineColumn (lin, col) = do
  ((currLin, currCol), remainingTxt) <- get
  let lineSplittedTxt = T.lines remainingTxt
      (linesBefore, linesAfter) = splitAt (lin - currLin) lineSplittedTxt
      ((txtInBreakLineBefore, txtInBreakLineAfter), linesAfterBreakLine) =
        case linesAfter of
          [] -> (("", ""), [])
          breakLine : xs ->
            if null linesBefore
              then (T.splitAt (col - currCol) breakLine, xs)
              else (T.splitAt (col - 1) breakLine, xs)
      txtBefore = T.intercalate "\n" (linesBefore ++ [txtInBreakLineBefore])
      txtAfter = T.intercalate "\n" (txtInBreakLineAfter : linesAfterBreakLine)
  put ((lin, col), txtAfter)
  pure txtBefore

isContainedIn :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Bool
(s1, e1) `isContainedIn` (s2, e2) = s1 >= s2 && e1 <= e2

sliceText :: (Int, Int) -> (Int, Int) -> State ((Int, Int), Text) Text
sliceText start end = do
  _ <- splitLineColumn start
  splitLineColumn end

findText :: Text -> Text -> Maybe (Int, Int)
findText needle haystick = do
  let (searched, remaining) = T.breakOn needle haystick
  guard (not (T.null remaining))
  let ls = T.lines searched
  case NE.nonEmpty ls of
    Nothing ->
      Just (0, 0)
    Just ls' ->
      Just (NE.length ls' - 1, T.length (NE.last ls'))

addLineCol :: (Int, Int) -> (Int, Int) -> (Int, Int)
addLineCol (i, j) (di, dj)
  | di == 0 = (i, j + dj)
  | otherwise = (i + di, dj)

filterTopLevel :: (Show a) => [(((Int, Int), (Int, Int)), a)] -> [(((Int, Int), (Int, Int)), a)]
filterTopLevel items = go [] items
  where
    go ys [] = ys
    go ys (x : xs) =
      let ys' = L.nubBy ((==) `on` fst) $ L.sortBy (compare `on` fst) $ go' ys x
       in go ys' xs
    go' [] x = [x]
    go' zs@(y : ys) x
      | fst x `isContainedIn` fst y = zs
      | fst y `isContainedIn` fst x = x : go' ys x
      | otherwise = y : go' ys x

reduceDeclRange :: Text -> ((Int, Int), (Int, Int)) -> Text -> Maybe ((Int, Int), (Int, Int))
reduceDeclRange src (start, end) needle =
  let (sliced, _) = runState (sliceText start end) ((1, 1), src)
      mdidj = findText needle sliced
      indexFromStart didj =
        let startOfNeedle = addLineCol start didj
            endOfNeedle = addLineCol startOfNeedle (0, T.length needle - 1)
         in (startOfNeedle, endOfNeedle)
   in fmap indexFromStart mdidj
