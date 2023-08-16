{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Layouter.Graph.Types
  ( -- * graph visualization information
    Point (..),
    HasPoint (..),
    toTuple,
    Dimension (..),
    HasDimension (..),
    NodeLayout (..),
    HasNodeLayout (..),
    EdgeLayout (..),
    HasEdgeLayout (..),
    GraphVisInfo (..),
    HasGraphVisInfo (..),
    transposeGraphVis,
  )
where

import Control.Lens (makeClassy)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Point = Point
  { _pointX :: Double,
    _pointY :: Double
  }
  deriving (Show, Generic)

makeClassy ''Point

instance FromJSON Point

instance ToJSON Point

toTuple :: Point -> (Double, Double)
toTuple (Point x y) = (x, y)

data Dimension = Dim
  { _dimWidth :: Double,
    _dimHeight :: Double
  }
  deriving (Show, Generic)

makeClassy ''Dimension

instance FromJSON Dimension

instance ToJSON Dimension

data NodeLayout a = NodeLayout
  { -- | information in node
    _nodePayload :: a,
    -- | node center position
    _nodePosition :: Point,
    -- | node width and height
    _nodeSize :: Dimension
  }
  deriving (Show, Generic)

makeClassy ''NodeLayout

instance FromJSON a => FromJSON (NodeLayout a)

instance ToJSON a => ToJSON (NodeLayout a)

data EdgeLayout = EdgeLayout
  { -- | edge id from the graph layouter
    _edgeId :: Int,
    -- | (source node, target node)
    _edgeEndNodes :: (Int, Int),
    -- | edge start point, end point
    _edgeStartEndPoints :: (Point, Point),
    -- | edge bend points
    _edgeBendPoints :: [Point]
  }
  deriving (Show, Generic)

makeClassy ''EdgeLayout

instance FromJSON EdgeLayout

instance ToJSON EdgeLayout

data GraphVisInfo = GraphVisInfo
  { _gviCanvasDim :: Dimension,
    _gviNodes :: [NodeLayout (Int, Text)],
    _gviEdges :: [EdgeLayout]
  }
  deriving (Show, Generic)

makeClassy ''GraphVisInfo

instance FromJSON GraphVisInfo

instance ToJSON GraphVisInfo

-- | swap horizontal and vertical directions.
transposeGraphVis :: GraphVisInfo -> GraphVisInfo
transposeGraphVis (GraphVisInfo dim nodLayout edgLayout) =
  GraphVisInfo dim' nodLayout' edgLayout'
  where
    xposeDim (Dim w h) = Dim h w
    xposePt (Point x y) = Point y x
    xposeNode (NodeLayout p xy d) = NodeLayout p (xposePt xy) (xposeDim d)
    xposeEdge (EdgeLayout i (j, k) (start, end) pts) =
      EdgeLayout i (j, k) (xposePt start, xposePt end) (fmap xposePt pts)
    dim' = xposeDim dim
    nodLayout' = fmap xposeNode nodLayout
    edgLayout' = fmap xposeEdge edgLayout
