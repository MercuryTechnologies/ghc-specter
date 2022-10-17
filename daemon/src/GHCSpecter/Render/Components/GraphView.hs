{-# LANGUAGE MultiWayIf #-}

module GHCSpecter.Render.Components.GraphView
  ( renderModuleGraph,
    renderGraph,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( classList,
    height,
    onClick,
    onMouseEnter,
    onMouseLeave,
    width,
  )
import Concur.Replica.SVG.Props qualified as SP
import Control.Lens ((^.), _1)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
import GHCSpecter.Channel.Common.Types
  ( DriverId (..),
    type ModuleName,
  )
import GHCSpecter.Channel.Outbound.Types (Timer)
import GHCSpecter.Data.Timing.Util (isModuleCompilationDone)
import GHCSpecter.GraphLayout.Types
  ( Dimension (..),
    EdgeLayout (..),
    GraphVisInfo (..),
    HasGraphVisInfo (..),
    HasNodeLayout (..),
    NodeLayout (..),
    Point (..),
  )
import GHCSpecter.Render.Util (xmlns)
import GHCSpecter.UI.ConcurReplica.DOM (div, text)
import GHCSpecter.UI.ConcurReplica.SVG qualified as S
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Types.Event (ModuleGraphEvent (..))
import GHCSpecter.Util.Map (BiKeyMap, KeyMap)
import Text.Printf (printf)
import Prelude hiding (div)

makePolylineText :: (Point, Point) -> [Point] -> Text
makePolylineText (p0, p1) xys =
  T.intercalate " " (fmap each ([p0] ++ xys ++ [p1]))
  where
    each (Point x y) = T.pack $ printf "%.2f,%.2f" x y

renderModuleGraph ::
  -- | key = graph id
  IntMap ModuleName ->
  BiKeyMap DriverId ModuleName ->
  KeyMap DriverId Timer ->
  [(Text, [Text])] ->
  GraphVisInfo ->
  -- | (focused (clicked), hinted (hovered))
  (Maybe Text, Maybe Text) ->
  Widget IHTML ModuleGraphEvent
renderModuleGraph
  nameMap
  drvModMap
  timing
  clustering
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
          let (color, swidth) = fromMaybe ("gray", "1") $ do
                hinted <- mhinted
                hintedIdx_ <- M.lookup hinted revNameMap
                hintedIdx <- Just hintedIdx_
                if
                    | src == hintedIdx -> pure ("black", "2")
                    | tgt == hintedIdx -> pure ("black", "2")
                    | otherwise -> Nothing
              -- if source and target nodes cannot be found,
              -- just use coordinates recorded in edge.
              -- TODO: should be handled as error.
              (srcPt, tgtPt) = fromMaybe (srcPt0, tgtPt0) $ do
                srcNode <- IM.lookup src nodeLayoutMap
                tgtNode <- IM.lookup tgt nodeLayoutMap
                -- Left-to-right flow.
                pure (rightCenter srcNode, leftCenter tgtNode)
           in S.polyline
                [ SP.points (makePolylineText (srcPt, tgtPt) xys)
                , SP.stroke color
                , SP.strokeWidth swidth
                , SP.fill "none"
                ]
                []

        box0 (NodeLayout (_, name) (Point x y) (Dim w h)) =
          let color
                | Just name == mfocused = "orange"
                | Just name == mhinted = "honeydew"
                | otherwise = "ivory"
           in S.rect
                [ HoverOnModuleEv (Just name) <$ onMouseEnter
                , HoverOnModuleEv Nothing <$ onMouseLeave
                , ClickOnModuleEv (Just name) <$ onClick
                , SP.x (T.pack $ show (x + offX))
                , SP.y (T.pack $ show (y + h * offYFactor + h - 6))
                , width (T.pack $ show (w * aFactor))
                , height "13"
                , SP.stroke "dimgray"
                , SP.fill color
                , SP.pointerEvents "visible"
                ]
                []
        box1 (NodeLayout _ (Point x y) (Dim w h)) =
          S.rect
            [ SP.x (T.pack $ show (x + offX))
            , SP.y (T.pack $ show (y + h * offYFactor + h + 3))
            , width (T.pack $ show (w * aFactor))
            , height "4"
            , SP.stroke "black"
            , SP.fill "none"
            , SP.pointerEvents "none"
            ]
            []
        box2 (NodeLayout (_, name) (Point x y) (Dim w h)) =
          let ratio = fromMaybe 0 $ do
                cluster <- L.lookup name clustering
                let nTot = length cluster
                if nTot == 0
                  then Nothing
                  else do
                    let compiled = filter (isModuleCompilationDone drvModMap timing) cluster
                        nCompiled = length compiled
                    pure (fromIntegral nCompiled / fromIntegral nTot)
              w' = ratio * w
           in S.rect
                [ SP.x (T.pack $ show (x + offX))
                , SP.y (T.pack $ show (y + h * offYFactor + h + 3))
                , width (T.pack $ show (w' * aFactor))
                , height "4"
                , SP.fill "blue"
                , SP.pointerEvents "none"
                ]
                []
        moduleText (NodeLayout (_, name) (Point x y) (Dim _w h)) =
          S.text
            [ SP.x (T.pack $ show (x + offX + 2))
            , SP.y (T.pack $ show (y + h * offYFactor + h))
            , classList [("small", True)]
            , SP.pointerEvents "none"
            ]
            [text name]
        edges = fmap edge (grVisInfo ^. gviEdges)
        nodes =
          concatMap (\x -> [box0 x, box1 x, box2 x, moduleText x]) (grVisInfo ^. gviNodes)

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
            (S.style [] [text ".small { font: 6px Courier,monospace; } text { user-select: none; }"] : (edges ++ nodes))
     in div [classList [("is-fullwidth", True)]] [svgElement]

-- | render graph more simply
renderGraph :: (Text -> Bool) -> GraphVisInfo -> Widget IHTML a
renderGraph cond grVisInfo =
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
        let (color, swidth) = ("gray", "1")
            -- if source and target nodes cannot be found,
            -- just use coordinates recorded in edge.
            -- TODO: should be handled as error.
            (srcPt, tgtPt) = fromMaybe (srcPt0, tgtPt0) $ do
              srcNode <- IM.lookup src nodeLayoutMap
              tgtNode <- IM.lookup tgt nodeLayoutMap
              -- Left-to-right flow.
              pure (rightCenter srcNode, leftCenter tgtNode)
         in S.polyline
              [ SP.points (makePolylineText (srcPt, tgtPt) xys)
              , SP.stroke color
              , SP.strokeWidth swidth
              , SP.fill "none"
              ]
              []

      box0 (NodeLayout (_, name) (Point x y) (Dim w h)) =
        let color
              | cond name = "honeydew"
              | otherwise = "ivory"
         in S.rect
              [ SP.x (T.pack $ show (x + offX))
              , SP.y (T.pack $ show (y + h * offYFactor + h - 6))
              , width (T.pack $ show (w * aFactor))
              , height "10"
              , SP.stroke "dimgray"
              , SP.fill color
              ]
              []
      labelText (NodeLayout (_, name) (Point x y) (Dim _w h)) =
        S.text
          [ SP.x (T.pack $ show (x + offX + 2))
          , SP.y (T.pack $ show (y + h * offYFactor + h))
          , classList [("small", True)]
          ]
          [text name]

      edges = fmap edge (grVisInfo ^. gviEdges)
      nodes =
        concatMap (\x -> [box0 x, labelText x]) (grVisInfo ^. gviNodes)

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
          (S.style [] [text ".small { font: 6px Courier,monospace; } text { user-select: none; }"] : (edges ++ nodes))
   in div [classList [("is-fullwidth", True)]] [svgElement]
