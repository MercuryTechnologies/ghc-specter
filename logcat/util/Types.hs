module Types (
  ClosureInfoItem (..),
) where

import Data.Text (Text)

data ClosureInfoItem = ClosureInfoItem
  { clsGraph :: [(Double, Double)]
  , clsN :: Int
  , clsLabel :: Text
  , clsDesc :: Text
  , clsCTy :: Text
  , clsType :: Text
  , clsModule :: Text
  , clsLoc :: Text
  , clsSize :: Double
  }
  deriving (Show)
