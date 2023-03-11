module View (
  computeLabelPositions,
  hitTest,
) where

import Data.List qualified as L
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
       in (tag, Rectangle x y 50 10)

hitTest :: (Double, Double) -> Map String Rectangle -> Maybe String
hitTest (x, y) posMap =
  fst
    <$> L.find (\(_, r) -> (x, y) `isInside` r) (Map.toAscList posMap)
  where
    isInside (x', y') (Rectangle x0 y0 w h) =
      x' >= x0 && x' <= x0 + w && y' >= y0 && y' <= y0 + h
