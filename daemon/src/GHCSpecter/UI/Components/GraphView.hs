{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.UI.Components.GraphView
  ( buildModuleGraph,
    buildGraph,
  )
where

import Control.Lens ((^.), _1)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Tuple (swap)
import GHCSpecter.Channel.Common.Types (type ModuleName)
import GHCSpecter.Graphics.DSL
  ( Color (..),
    HitEvent (..),
    Primitive (..),
    Scene (..),
    TextFontFace (..),
    TextPosition (..),
    ViewPort (..),
    polyline,
    rectangle,
  )
import GHCSpecter.Layouter.Graph.Types
  ( Dimension (..),
    EdgeLayout (..),
    GraphVisInfo (..),
    HasGraphVisInfo (..),
    HasNodeLayout (..),
    NodeLayout (..),
    Point (..),
    toTuple,
  )
import GHCSpecter.Layouter.Text
  ( MonadTextLayout,
    drawText',
  )
import GHCSpecter.UI.Types.Event (ModuleGraphEvent (..))
import Prelude hiding (div)

buildModuleGraph ::
  forall m.
  (MonadTextLayout m) =>
  -- | key = graph id
  IntMap ModuleName ->
  -- | For each module, assign a double type value in [0, 1],
  -- which will be shown as bar below the module node.
  (Text -> Double) ->
  -- | Graph layout information
  GraphVisInfo ->
  -- | (focused (clicked), hinted (hovered))
  (Maybe Text, Maybe Text) ->
  m (Scene (Primitive ModuleGraphEvent))
buildModuleGraph
  nameMap
  valueFor
  grVisInfo
  (mfocused, mhinted) = do
    let Dim canvasWidth canvasHeight = grVisInfo ^. gviCanvasDim
        extent = ViewPort (0, 0) (canvasWidth + 100, canvasHeight + 100)
        revNameMap = M.fromList $ fmap swap $ IM.toList nameMap
        nodeLayoutMap =
          IM.fromList $ fmap (\n -> (n ^. nodePayload . _1, n)) (grVisInfo ^. gviNodes)
        -- graph layout parameter
        aFactor = 1.05
        offX = -15
        offYFactor = -1.0
        -- the center of left side of a node
        leftCenter (NodeLayout _ (Point x y) (Dim _ h)) =
          Point (x + offX) (y + h * offYFactor + h + 0.5)
        -- the center of right side of a node
        rightCenter (NodeLayout _ (Point x y) (Dim w h)) =
          Point (x + offX + w * aFactor) (y + h * offYFactor + h + 0.5)
        edge (EdgeLayout _ (src, tgt) (srcPt0, tgtPt0) xys) =
          let (color, swidth) = fromMaybe (Gray, 1.0) $ do
                hinted <- mhinted
                hintedIdx_ <- M.lookup hinted revNameMap
                hintedIdx <- Just hintedIdx_
                if
                    | src == hintedIdx -> pure (Black, 2.0)
                    | tgt == hintedIdx -> pure (Black, 2.0)
                    | otherwise -> Nothing
              -- if source and target nodes cannot be found,
              -- just use coordinates recorded in edge.
              -- TODO: should be handled as error.
              (srcPt, tgtPt) = fromMaybe (srcPt0, tgtPt0) $ do
                srcNode <- IM.lookup src nodeLayoutMap
                tgtNode <- IM.lookup tgt nodeLayoutMap
                -- Left-to-right flow.
                pure (rightCenter srcNode, leftCenter tgtNode)
           in polyline (toTuple srcPt) (fmap toTuple xys) (toTuple tgtPt) color swidth
        node (NodeLayout (_, name) (Point x y) (Dim w h)) = do
          let fontSize = 5
              ratio = valueFor name
              w' = ratio * w
              color1
                | Just name == mfocused = Orange
                | Just name == mhinted = HoneyDew
                | otherwise = Ivory
              hitEvent =
                HitEvent
                  { hitEventHoverOn = Just (HoverOnModuleEv (Just name)),
                    hitEventHoverOff = Just (HoverOnModuleEv Nothing),
                    hitEventClick = Just (Right (ClickOnModuleEv (Just name)))
                  }
          renderedText <- drawText' (x + offX + 2, y + h * offYFactor + h) LowerLeft Sans Black fontSize name
          -- TODO: width and height should be replaced by correct impl.
          pure
            [ rectangle (x + offX, y + h * offYFactor + h - 6) (w * aFactor) 13 (Just DimGray) (Just color1) (Just 0.8) (Just hitEvent),
              rectangle (x + offX, y + h * offYFactor + h + 3) (w * aFactor) 4 (Just Black) (Just White) (Just 0.8) Nothing,
              rectangle (x + offX, y + h * offYFactor + h + 3) (w' * aFactor) 4 Nothing (Just Blue) Nothing Nothing,
              renderedText
            ]
    renderedNodes <-
      concat <$> traverse node (grVisInfo ^. gviNodes)
    pure
      Scene
        { sceneId = "main-module-graph",
          sceneGlobalViewPort = extent,
          sceneLocalViewPort = extent,
          sceneElements =
            [rectangle (0, 0) canvasWidth canvasHeight Nothing Nothing Nothing Nothing] -- just dummy for now
              ++ fmap edge (grVisInfo ^. gviEdges)
              ++ renderedNodes,
          sceneExtents = Just extent
        }

-- | build graph more simply to graphics DSL
buildGraph ::
  forall m.
  (MonadTextLayout m) =>
  (Text -> Bool) ->
  GraphVisInfo ->
  m [Primitive ModuleGraphEvent]
buildGraph cond grVisInfo = do
  let Dim canvasWidth canvasHeight = grVisInfo ^. gviCanvasDim
      nodeLayoutMap =
        IM.fromList $ fmap (\n -> (n ^. nodePayload . _1, n)) (grVisInfo ^. gviNodes)
      -- graph layout parameter
      aFactor = 1.5
      offX = -15
      offYFactor = -1.0
      -- the center of left side of a node
      leftCenter (NodeLayout _ (Point x y) (Dim _ h)) =
        Point (x + offX) (y + h * offYFactor + h + 0.5)
      -- the center of right side of a node
      rightCenter (NodeLayout _ (Point x y) (Dim w h)) =
        Point (x + offX + w * aFactor) (y + h * offYFactor + h + 0.5)
      edge (EdgeLayout _ (src, tgt) (srcPt0, tgtPt0) xys) =
        let (color, swidth) = (Gray, 1.0)
            -- if source and target nodes cannot be found,
            -- just use coordinates recorded in edge.
            -- TODO: should be handled as error.
            (srcPt, tgtPt) = fromMaybe (srcPt0, tgtPt0) $ do
              srcNode <- IM.lookup src nodeLayoutMap
              tgtNode <- IM.lookup tgt nodeLayoutMap
              -- Left-to-right flow.
              pure (rightCenter srcNode, leftCenter tgtNode)
         in polyline (toTuple srcPt) (fmap toTuple xys) (toTuple tgtPt) color swidth

      node (NodeLayout (_, name) (Point x y) (Dim w h)) = do
        let fontSize = 5
            color
              | cond name = HoneyDew
              | otherwise = Ivory
        renderedText <-
          drawText' (x + offX + 2, y + h * offYFactor + h) LowerLeft Sans Black fontSize name
        pure
          [ rectangle (x + offX, y + h * offYFactor + h - 6) (w * aFactor) 10 (Just DimGray) (Just color) (Just 0.8) Nothing,
            renderedText
          ]
  renderedNodes <-
    concat <$> traverse node (grVisInfo ^. gviNodes)
  pure $
    [rectangle (0, 0) canvasWidth canvasHeight Nothing Nothing Nothing Nothing] -- just dummy for now
      ++ fmap edge (grVisInfo ^. gviEdges)
      ++ renderedNodes
