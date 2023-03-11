module View (
  computeLabelPositions,
) where

import Data.Map (Map)
import Data.Map qualified as Map
import Types (Rectangle (..))
import Util.Event (eventInfoEnumMap)

computeLabelPositions :: (Double, Double) -> Map String Rectangle
computeLabelPositions (xoffset, yoffset) = Map.fromList $ fmap calc eventInfoEnumMap
  where
    calc (tag, tagEnum) =
      let x = xoffset
          y = yoffset + 10.0 * fromIntegral tagEnum
       in (tag, Rectangle x y (x + 50) (y + 10))
