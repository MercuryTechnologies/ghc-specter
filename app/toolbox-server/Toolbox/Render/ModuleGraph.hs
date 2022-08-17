{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Toolbox.Render.ModuleGraph
  ( layOutGraph,
    renderModuleGraph,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( Props,
    classList,
    div,
    height,
    onClick,
    onMouseEnter,
    onMouseLeave,
    pre,
    text,
    textProp,
    width,
  )
import qualified Concur.Replica.SVG as S
import qualified Concur.Replica.SVG.Props as SP
import Control.Monad (void)
import Control.Monad.Extra (loop)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (allocate)
import Data.Bits ((.|.))
import qualified Data.Foldable as F
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)
import OGDF.Graph
  ( Graph,
    graph_newEdge,
    newGraph,
  )
import OGDF.GraphAttributes
  ( GraphAttributes,
    newGraphAttributes,
  )
import OGDF.NodeElement (nodeElement_index)
import PyF (fmt)
import Replica.VDOM.Types (HTML)
import STD.Deletable (delete)
import Toolbox.Channel
  ( ModuleGraphInfo (..),
    ModuleName,
    SessionInfo (..),
    Timer,
  )
import Toolbox.Server.Types
  ( Dimension (..),
    EdgeLayout (..),
    Event (..),
    GraphVisInfo (..),
    NodeLayout (..),
    Point (..),
    ServerState (..),
    UIState (..),
  )
import Toolbox.Util.Graph
  ( filterOutSmallNodes,
    mkRevDep,
  )
import Toolbox.Util.OGDF
  ( appendText,
    doSugiyamaLayout,
    edgeGraphics,
    getAllEdgeLayout,
    getAllNodeLayout,
    getCanvasDim,
    newGraphNodeWithSize,
    nodeGraphics,
    nodeLabel,
    nodeStyle,
    runGraphLayouter,
  )
import Prelude hiding (div)

xmlns :: Props a
xmlns = textProp "xmlns" "http://www.w3.org/2000/svg"

analyze :: ModuleGraphInfo -> Text
analyze graphInfo =
  let modDep = mginfoModuleDep graphInfo
      modRevDep = mkRevDep modDep
      initials = IM.keys $ IM.filter (\js -> null js) modDep
      terminals = IM.keys $ IM.filter (\js -> null js) modRevDep
      orphans = initials `L.intersect` terminals
      singles = IM.mapMaybe (\js -> case js of j : [] -> Just j; _ -> Nothing) modDep
      leg i = loop go ([i], i)
        where
          go (acc', i') =
            case IM.lookup i' singles of
              Nothing -> Right acc'
              Just j' -> Left (acc' ++ [j'], j')
      legs = fmap leg (initials L.\\ orphans)
      larges = filterOutSmallNodes graphInfo
      largeNames = mapMaybe (\i -> IM.lookup i (mginfoModuleNameMap graphInfo)) larges
   in "intials: " <> (T.pack $ show initials) <> ",\n"
        <> "terminals: "
        <> (T.pack $ show terminals)
        <> ",\n"
        <> "orphans: "
        <> (T.pack $ show orphans)
        <> ",\n"
        <> "singles: "
        <> (T.pack $ show singles)
        <> ",\n"
        <> "legs: "
        <> (T.pack $ show legs)
        <> "\n=============\n"
        <> "larges: "
        <> (T.pack $ show largeNames)
        <> "# of larges: "
        <> (T.pack $ show (length larges))

-- | (number of vertices, number of edges)
stat :: ModuleGraphInfo -> (Int, Int)
stat mgi =
  let nVtx = F.length $ mginfoModuleNameMap mgi
      nEdg = F.sum $ fmap length $ mginfoModuleDep mgi
   in (nVtx, nEdg)

formatModuleGraphInfo :: ModuleGraphInfo -> Text
formatModuleGraphInfo mgi =
  let txt1 =
        T.intercalate "\n" . fmap (T.pack . show) $ IM.toList $ mginfoModuleNameMap mgi
      txt2 =
        T.intercalate "\n" . fmap (T.pack . show) $ IM.toList $ mginfoModuleDep mgi
      txt3 =
        T.pack . show $ mginfoModuleTopSorted mgi
      (nVtx, nEdg) = stat mgi
   in "(key, module):\n"
        <> txt1
        <> "\n-----------------\n"
        <> "dependencies:\n"
        <> txt2
        <> "\n-----------------\n"
        <> "top sorted:\n"
        <> txt3
        <> "\n=================\n"
        <> analyze mgi
        <> "\n=================\n"
        <> "# of vertices: "
        <> T.pack (show nVtx)
        <> ", # of edges: "
        <> T.pack (show nEdg)

makePolylineText :: [Point] -> Text
makePolylineText xys = T.intercalate " " (fmap each xys)
  where
    each (Point x y) = [fmt|{x:.2},{y:.2}|]

renderModuleGraphSVG ::
  Map Text Timer ->
  [(Text, [Text])] ->
  GraphVisInfo ->
  Maybe Text ->
  Widget HTML Event
renderModuleGraphSVG timing clustering grVisInfo mhovered =
  let Dim canvasWidth canvasHeight = gviCanvasDim grVisInfo
      edge (EdgeLayout _ _ xys) =
        S.polyline
          [ SP.points (makePolylineText xys)
          , SP.stroke "gray"
          , SP.fill "none"
          ]
          []
      aFactor = 0.8
      offXFactor = -0.4
      offYFactor = -0.6
      box0 (NodeLayout name (Point x y) (Dim w h)) =
        S.rect
          [ HoverOnModuleEv (Just name) <$ onMouseEnter
          , HoverOnModuleEv Nothing <$ onMouseLeave
          , ClickOnModuleEv (Just name) <$ onClick
          , SP.x (T.pack $ show (x + w * offXFactor))
          , SP.y (T.pack $ show (y + h * offYFactor + h - 12))
          , width (T.pack $ show (w * aFactor))
          , height "20"
          , SP.stroke "dimgray"
          , SP.fill $ if Just name == mhovered then "honeydew" else "ivory"
          ]
          []
      box1 (NodeLayout _ (Point x y) (Dim w h)) =
        S.rect
          [ SP.x (T.pack $ show (x + w * offXFactor))
          , SP.y (T.pack $ show (y + h * offYFactor + h + 3))
          , width (T.pack $ show (w * aFactor))
          , height "5"
          , SP.stroke "black"
          , SP.fill "none"
          ]
          []
      box2 (NodeLayout name (Point x y) (Dim w h)) =
        let ratio = fromMaybe 0 $ do
              cluster <- L.lookup name clustering
              let nTot = length cluster
              if nTot == 0
                then Nothing
                else do
                  let compiled = filter (\j -> j `M.member` timing) cluster
                      nCompiled = length compiled
                  pure (fromIntegral nCompiled / fromIntegral nTot)
            w' = ratio * w
         in S.rect
              [ SP.x (T.pack $ show (x + w * offXFactor))
              , SP.y (T.pack $ show (y + h * offYFactor + h + 3))
              , width (T.pack $ show (w' * aFactor))
              , height "5"
              , SP.fill "blue"
              ]
              []
      moduleText (NodeLayout name (Point x y) (Dim w h)) =
        S.text
          [ SP.x (T.pack $ show (x + w * offXFactor))
          , SP.y (T.pack $ show (y + h * offYFactor + h))
          , classList [("small", True)]
          ]
          [text name]

      edges = fmap edge $ gviEdges grVisInfo
      nodes =
        concatMap (\x -> [box0 x, box1 x, box2 x, moduleText x]) (gviNodes grVisInfo)

      svgElement =
        S.svg
          [ width "100%"
          , SP.viewBox
              ( "0 0 "
                  <> T.pack (show (canvasWidth + 100)) -- i don't understand why it's incorrect
                  <> " "
                  <> T.pack (show (canvasHeight + 100))
              )
          , SP.version "1.1"
          , xmlns
          ]
          (S.style [] [text ".small { font: 12px sans-serif; }"] : (edges ++ nodes))
   in div [classList [("is-fullwidth", True)]] [svgElement]

renderModuleGraph :: UIState -> ServerState -> Widget HTML Event
renderModuleGraph ui ss =
  let sessionInfo = serverSessionInfo ss
      timing = serverTiming ss
      clustering = serverModuleClustering ss
   in case sessionStartTime sessionInfo of
        Nothing ->
          pre [] [text "GHC Session has not been started"]
        Just _ ->
          div
            []
            ( ( case serverModuleGraph ss of
                  Nothing -> []
                  Just grVisInfo ->
                    [ renderModuleGraphSVG timing clustering grVisInfo (uiModuleHover ui)
                    , text (T.pack $ show (uiModuleClick ui))
                    ]
              )
                ++ [pre [] [text $ formatModuleGraphInfo (sessionModuleGraph sessionInfo)]]
            )

newGA :: Graph -> IO GraphAttributes
newGA g = newGraphAttributes g (nodeGraphics .|. edgeGraphics .|. nodeLabel .|. nodeStyle)

layOutGraph :: IntMap ModuleName -> IntMap [Int] -> IO GraphVisInfo
layOutGraph nameMap graph = runGraphLayouter $ do
  (_, g) <- allocate newGraph delete
  (_, ga) <- allocate (newGA g) delete

  moduleNodeMap <-
    flip IM.traverseMaybeWithKey graph $ \i _ -> do
      case IM.lookup i nameMap of
        Nothing -> pure Nothing
        Just name -> do
          let w = 8 * T.length name
          node <- newGraphNodeWithSize (g, ga) (w, 15)
          appendText ga node name
          pure (Just node)
  moduleNodeIndex <-
    traverse (\node -> liftIO (fromIntegral @_ @Int <$> nodeElement_index node)) moduleNodeMap
  let moduleNodeRevIndex = IM.fromList $ fmap swap $ IM.toList moduleNodeIndex
  void $
    flip IM.traverseWithKey graph $ \i js ->
      F.for_ (IM.lookup i moduleNodeMap) $ \node_i -> do
        let node_js = mapMaybe (\j -> IM.lookup j moduleNodeMap) js
        F.for_ node_js $ \node_j ->
          liftIO (graph_newEdge g node_i node_j)

  doSugiyamaLayout ga

  canvasDim <- getCanvasDim ga

  nodeLayout0 <- getAllNodeLayout g ga
  let nodeLayout = mapMaybe replace nodeLayout0
        where
          replace (NodeLayout j pt dim) = do
            i <- IM.lookup j moduleNodeRevIndex
            name <- IM.lookup i nameMap
            pure $ NodeLayout name pt dim

  edgeLayout <- getAllEdgeLayout g ga
  pure $ GraphVisInfo canvasDim nodeLayout edgeLayout
