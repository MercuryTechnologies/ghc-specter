{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module GHCSpecter.Render.ModuleGraph
  ( -- * Render HTML for the Module Graph tab
    render,

    -- * show textual info:

    -- TODO: they are obsolete and will be moved to a separate module
    formatModuleGraphInfo,
    stat,
    analyze,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( classList,
    onInput,
    style,
  )
import Concur.Replica.DOM.Props qualified as DP (checked, name, type_)
import Control.Error.Util (note)
import Control.Lens (to, (^.), _1)
import Control.Monad.Extra (loop)
import Data.Foldable qualified as F
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.Channel.Common.Types (DriverId, type ModuleName)
import GHCSpecter.Channel.Outbound.Types
  ( ModuleGraphInfo (..),
    SessionInfo (..),
    Timer,
  )
import GHCSpecter.GraphLayout.Algorithm.Builder (makeRevDep)
import GHCSpecter.GraphLayout.Algorithm.Cluster (filterOutSmallNodes)
import GHCSpecter.GraphLayout.Types
  ( GraphVisInfo (..),
    HasGraphVisInfo (..),
    NodeLayout (..),
  )
import GHCSpecter.Render.Components.GraphView qualified as GraphView
import GHCSpecter.Server.Types
  ( HasModuleGraphState (..),
    HasServerState (..),
    ServerState (..),
  )
import GHCSpecter.UI.ConcurReplica.DOM
  ( div,
    input,
    label,
    pre,
    text,
  )
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Constants (widgetHeight)
import GHCSpecter.UI.Types
  ( HasModuleGraphUI (..),
    HasUIModel (..),
    ModuleGraphUI (..),
    UIModel,
  )
import GHCSpecter.UI.Types.Event
  ( DetailLevel (..),
    Event (..),
    SubModuleEvent (..),
  )
import GHCSpecter.Util.Map (BiKeyMap, KeyMap)
import Text.Printf (printf)
import Prelude hiding (div)

analyze :: ModuleGraphInfo -> Text
analyze graphInfo =
  let modDep = mginfoModuleDep graphInfo
      modRevDep = makeRevDep modDep
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
      larges = filterOutSmallNodes modDep
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

renderMainModuleGraph ::
  -- | key = graph id
  IntMap ModuleName ->
  BiKeyMap DriverId ModuleName ->
  KeyMap DriverId Timer ->
  [(Text, [Text])] ->
  GraphVisInfo ->
  -- | main module graph UI state
  ModuleGraphUI ->
  Widget IHTML Event
renderMainModuleGraph
  nameMap
  drvModMap
  timing
  clustering
  grVisInfo
  mgUI =
    div
      [classList [("box", True)]]
      [ MainModuleEv
          <$> GraphView.renderModuleGraphSVG
            nameMap
            drvModMap
            timing
            clustering
            grVisInfo
            (mclicked, mhovered)
      ]
    where
      mclicked = mgUI ^. modGraphUIClick
      mhovered = mgUI ^. modGraphUIHover

renderSubModuleGraph ::
  -- | key = graph id
  IntMap ModuleName ->
  BiKeyMap DriverId ModuleName ->
  KeyMap DriverId Timer ->
  [(DetailLevel, [(ModuleName, GraphVisInfo)])] ->
  -- | (main module graph UI state, sub module graph UI state)
  (ModuleGraphUI, (DetailLevel, ModuleGraphUI)) ->
  Widget IHTML Event
renderSubModuleGraph
  nameMap
  drvModMap
  timing
  subgraphs
  (mainMGUI, (detailLevel, subMGUI)) =
    let mainModuleClicked = mainMGUI ^. modGraphUIClick
        subModuleHovered = subMGUI ^. modGraphUIHover
        esubgraph = do
          selected <-
            note "no module cluster is selected" mainModuleClicked
          subgraphsAtTheLevel <-
            note (printf "%s subgraph is not computed" (show detailLevel)) (L.lookup detailLevel subgraphs)
          subgraph <-
            note
              (printf "cannot find the subgraph for the module cluster %s" (T.unpack selected))
              (L.lookup selected subgraphsAtTheLevel)
          pure subgraph
     in case esubgraph of
          Left err -> text (T.pack err)
          Right subgraph ->
            let tempclustering =
                  fmap
                    (\(NodeLayout (_, name) _ _) -> (name, [name]))
                    (subgraph ^. gviNodes)
             in div
                  [classList [("box", True)]]
                  [ SubModuleEv . SubModuleGraphEv
                      <$> GraphView.renderModuleGraphSVG
                        nameMap
                        drvModMap
                        timing
                        tempclustering
                        subgraph
                        (mainModuleClicked, subModuleHovered)
                  ]

renderDetailLevel :: UIModel -> Widget IHTML Event
renderDetailLevel model =
  SubModuleEv . SubModuleLevelEv
    <$> div
      [classList [("control", True)]]
      [detail30, detail100, detail300]
  where
    currLevel = model ^. modelSubModuleGraph . _1
    mkRadioItem ev txt isChecked =
      label
        [classList [("radio", True)]]
        [ input [DP.type_ "radio", DP.name "detail", DP.checked isChecked, ev <$ onInput]
        , text txt
        ]

    detail30 = mkRadioItem UpTo30 "< 30" (currLevel == UpTo30)
    detail100 = mkRadioItem UpTo100 "< 100" (currLevel == UpTo100)
    detail300 = mkRadioItem UpTo300 "< 300" (currLevel == UpTo300)

-- | top-level render function for Module Graph tab
render :: UIModel -> ServerState -> Widget IHTML Event
render model ss =
  case sessionStartTime sessionInfo of
    Nothing ->
      pre [] [text "GHC Session has not been started"]
    Just _ ->
      div
        [ style
            [ ("overflow", "scroll")
            , ("height", ss ^. serverSessionInfo . to sessionIsPaused . to widgetHeight)
            ]
        ]
        ( case mgs ^. mgsClusterGraph of
            Nothing -> []
            Just grVisInfo ->
              [ renderMainModuleGraph
                  nameMap
                  drvModMap
                  timing
                  clustering
                  grVisInfo
                  (model ^. modelMainModuleGraph)
              , renderDetailLevel model
              , renderSubModuleGraph
                  nameMap
                  drvModMap
                  timing
                  (mgs ^. mgsSubgraph)
                  (model ^. modelMainModuleGraph, model ^. modelSubModuleGraph)
              ]
        )
  where
    sessionInfo = ss ^. serverSessionInfo
    nameMap = mginfoModuleNameMap $ sessionModuleGraph sessionInfo
    drvModMap = ss ^. serverDriverModuleMap
    timing = ss ^. serverTiming
    mgs = ss ^. serverModuleGraphState
    clustering = mgs ^. mgsClustering
