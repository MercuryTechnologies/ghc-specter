{-# LANGUAGE OverloadedStrings #-}

module Extract (
  EventlogItem (..),
  extract,
) where

import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Text.HTML.TagSoup (
  Tag (..),
  parseTags,
 )
import Text.HTML.TagSoup.Tree (
  TagTree (..),
  renderTree,
  tagTree,
  universeTree,
 )

data EventlogItem = EventlogItem
  { eventGraph :: [(Double, Double)]
  , eventN :: Int
  , eventLabel :: Text
  , eventDesc :: Text
  , eventCTy :: Text
  , eventType :: Text
  , eventModule :: Text
  , eventLoc :: Text
  , eventSize :: Double
  }
  deriving (Show)

readT :: (Read a) => Text -> a
readT = read . T.unpack

parseLineChart :: Text -> [(Double, Double)]
parseLineChart txt =
  let xs = T.split (== ',') txt
      parse x =
        let [y1, y2] = T.split (== ':') x
         in (readT y1, readT y2)
   in fmap parse xs

getLineChart x = listToMaybe $ do
  TagBranch "td" _ xs <- [x]
  TagBranch "span" _ ys <- xs
  TagLeaf (TagText contents) <- ys
  pure (parseLineChart contents)

getText :: TagTree T.Text -> Maybe T.Text
getText x = listToMaybe $ do
  TagBranch "td" _ xs <- [x]
  TagLeaf (TagText content) <- xs
  pure content

extract :: FilePath -> IO [EventlogItem]
extract fp = do
  txt <- TIO.readFile fp
  let tags = parseTags txt
      trs = tagTree tags
      convert (c1 : c2 : c3 : c4 : c5 : c6 : c7 : c8 : c9 : _) =
        EventlogItem
          <$> getLineChart c1
          <*> (readT @Int <$> getText c2)
          <*> getText c3
          <*> getText c4
          <*> getText c5
          <*> getText c6
          <*> getText c7
          <*> getText c8
          <*> (readT @Double <$> getText c9)
      infos = do
        TagBranch "table" _ rows <- universeTree trs
        TagBranch "tr" _ cols <- rows
        pure (convert cols)
  pure $ catMaybes infos
