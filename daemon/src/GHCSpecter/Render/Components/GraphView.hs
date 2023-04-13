{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Render.Components.GraphView (
  compileModuleGraph,
  compileGraph,
  renderModuleGraph,
  renderGraph,
) where

import Concur.Core (Widget)
import Concur.Replica (
  classList,
  onClick,
  onMouseEnter,
  onMouseLeave,
  width,
 )
import Concur.Replica.SVG.Props qualified as SP
import Control.Lens ((^.), _1)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
import GHCSpecter.Channel.Common.Types (type ModuleName)
import GHCSpecter.GraphLayout.Types (
  Dimension (..),
  EdgeLayout (..),
  GraphVisInfo (..),
  HasGraphVisInfo (..),
  HasNodeLayout (..),
  NodeLayout (..),
  Point (..),
  toTuple,
 )
import GHCSpecter.Graphics.DSL (
  Color (..),
  Primitive (..),
  Scene (..),
  TextFontFace (..),
  TextPosition (..),
  ViewPort (..),
 )
import GHCSpecter.Render.ConcurReplicaSVG (renderPrimitive)
import GHCSpecter.Render.Util (xmlns)
import GHCSpecter.UI.ConcurReplica.DOM (div, text)
import GHCSpecter.UI.ConcurReplica.SVG qualified as S
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Types.Event (ModuleGraphEvent (..))
import Prelude hiding (div)

compileModuleGraph ::
  -- | key = graph id
  IntMap ModuleName ->
  -- | For each module, assign a double type value in [0, 1],
  -- which will be shown as bar below the module node.
  (Text -> Double) ->
  -- | Graph layout information
  GraphVisInfo ->
  -- | (focused (clicked), hinted (hovered))
  (Maybe Text, Maybe Text) ->
  Scene
compileModuleGraph
  nameMap
  valueFor
  grVisInfo
  (mfocused, mhinted) =
    let Dim canvasWidth canvasHeight = grVisInfo ^. gviCanvasDim
        revNameMap = M.fromList $ fmap swap $ IM.toList nameMap
        nodeLayoutMap =
          IM.fromList $ fmap (\n -> (n ^. nodePayload . _1, n)) (grVisInfo ^. gviNodes)
        -- graph layout parameter
        aFactor = 0.95
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
           in Polyline (toTuple srcPt) (fmap toTuple xys) (toTuple tgtPt) color swidth
        node (NodeLayout (_, name) (Point x y) (Dim w h)) =
          let fontSize = 6
              ratio = valueFor name
              w' = ratio * w
              color1
                | Just name == mfocused = Orange
                | Just name == mhinted = HoneyDew
                | otherwise = Ivory
           in [ Rectangle (x + offX, y + h * offYFactor + h - 6) (w * aFactor) 13 (Just DimGray) (Just color1) (Just 0.8) (Just name)
              , Rectangle (x + offX, y + h * offYFactor + h + 3) (w * aFactor) 4 (Just Black) (Just White) (Just 0.8) Nothing
              , Rectangle (x + offX, y + h * offYFactor + h + 3) (w' * aFactor) 4 Nothing (Just Blue) Nothing Nothing
              , DrawText (x + offX + 2, y + h * offYFactor + h) LowerLeft Sans Black fontSize name
              ]
     in Scene
          { sceneId = "main-module-graph"
          , sceneGlobalViewPort = ViewPort (0, 0) (canvasWidth, canvasHeight)
          , sceneLocalViewPort = ViewPort (0, 0) (canvasWidth, canvasHeight)
          , sceneElements =
              [Rectangle (0, 0) canvasWidth canvasHeight Nothing Nothing Nothing Nothing] -- just dummy for now
                ++ fmap edge (grVisInfo ^. gviEdges)
                ++ concatMap node (grVisInfo ^. gviNodes)
          }

-- | compile graph more simply to graphics DSL
compileGraph :: (Text -> Bool) -> GraphVisInfo -> [Primitive]
compileGraph cond grVisInfo =
  let Dim canvasWidth canvasHeight = grVisInfo ^. gviCanvasDim
      nodeLayoutMap =
        IM.fromList $ fmap (\n -> (n ^. nodePayload . _1, n)) (grVisInfo ^. gviNodes)
      -- graph layout parameter
      aFactor = 0.95
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
         in Polyline (toTuple srcPt) (fmap toTuple xys) (toTuple tgtPt) color swidth

      node (NodeLayout (_, name) (Point x y) (Dim w h)) =
        let fontSize = 6
            color
              | cond name = HoneyDew
              | otherwise = Ivory
         in [ Rectangle (x + offX, y + h * offYFactor + h - 6) (w * aFactor) 10 (Just DimGray) (Just color) (Just 0.8) Nothing
            , DrawText (x + offX + 2, y + h * offYFactor + h) LowerLeft Sans Black fontSize name
            ]
   in [Rectangle (0, 0) canvasWidth canvasHeight Nothing Nothing Nothing Nothing] -- just dummy for now
        ++ fmap edge (grVisInfo ^. gviEdges)
        ++ concatMap node (grVisInfo ^. gviNodes)

renderModuleGraph ::
  -- | key = graph id
  IntMap ModuleName ->
  -- | For each module, assign a double type value in [0, 1],
  -- which will be shown as bar below the module node.
  (ModuleName -> Double) ->
  -- | Graph layout information
  GraphVisInfo ->
  -- | (focused (clicked), hinted (hovered))
  (Maybe Text, Maybe Text) ->
  Widget IHTML ModuleGraphEvent
renderModuleGraph
  nameMap
  valueFor
  grVisInfo
  (mfocused, mhinted) =
    let Dim canvasWidth canvasHeight = grVisInfo ^. gviCanvasDim
        handlers name =
          [ HoverOnModuleEv (Just name) <$ onMouseEnter
          , HoverOnModuleEv Nothing <$ onMouseLeave
          , ClickOnModuleEv (Just name) <$ onClick
          ]
        scene = compileModuleGraph nameMap valueFor grVisInfo (mfocused, mhinted)
        rexp = sceneElements scene
        svgProps =
          [ width (T.pack (show (canvasWidth + 100)))
          , SP.viewBox
              ( "0 0 "
                  <> T.pack (show (canvasWidth + 100))
                  <> " "
                  <> T.pack (show (canvasHeight + 100))
              )
          , SP.version "1.1"
          , xmlns
          ]
        svgElement =
          S.svg
            svgProps
            ( S.style [] [text ".small { font: 6px Courier,monospace; } text { user-select: none; }"]
                : fmap (renderPrimitive handlers) rexp
            )
     in div [classList [("is-fullwidth", True)]] [svgElement]

-- | render graph more simply
renderGraph :: (Text -> Bool) -> GraphVisInfo -> Widget IHTML a
renderGraph cond grVisInfo =
  let Dim canvasWidth canvasHeight = grVisInfo ^. gviCanvasDim
      rexp = compileGraph cond grVisInfo
      svgProps =
        [ width (T.pack (show (canvasWidth + 100)))
        , SP.viewBox
            ( "0 0 "
                <> T.pack (show (canvasWidth + 100))
                <> " "
                <> T.pack (show (canvasHeight + 100))
            )
        , SP.version "1.1"
        , xmlns
        ]
      svgElement =
        S.svg
          svgProps
          ( S.style [] [text ".small { font: 6px Courier,monospace; } text { user-select: none; }"]
              : fmap (renderPrimitive (\_ -> [])) rexp
          )
   in div [classList [("is-fullwidth", True)]] [svgElement]
