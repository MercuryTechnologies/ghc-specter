{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module GHCSpecter.Render.ModuleGraph
  ( -- * Render HTML for the Module Graph tab
    render,
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
import Control.Lens (to, (^.), _1, _2)
import Data.IntMap (IntMap)
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.Channel.Common.Types (DriverId, type ModuleName)
import GHCSpecter.Channel.Outbound.Types
  ( ModuleGraphInfo (..),
    SessionInfo (..),
    Timer,
  )
import GHCSpecter.GraphLayout.Types
  ( GraphVisInfo (..),
    HasGraphVisInfo (..),
    HasNodeLayout (..),
  )
import GHCSpecter.Render.Components.GraphView qualified as GraphView
import GHCSpecter.Server.Types
  ( HasModuleGraphState (..),
    HasServerState (..),
    HasTimingState (..),
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
      [ classList [("box", True)]
      , style [("overflow", "scroll")]
      ]
      [ MainModuleEv
          <$> GraphView.renderModuleGraph
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
            let trivialClustering =
                  fmap
                    (\n -> let name = n ^. nodePayload . _2 in (name, [name]))
                    (subgraph ^. gviNodes)
             in div
                  [ classList [("box", True)]
                  , style [("overflow", "scroll")]
                  ]
                  [ SubModuleEv . SubModuleGraphEv
                      <$> GraphView.renderModuleGraph
                        nameMap
                        drvModMap
                        timing
                        trivialClustering
                        subgraph
                        (mainModuleClicked, subModuleHovered)
                  ]

renderDetailLevel :: UIModel -> Widget IHTML Event
renderDetailLevel model =
  SubModuleEv . SubModuleLevelEv
    <$> div
      [ classList [("control", True)]
      , style [("position", "absolute"), ("top", "0"), ("right", "0")]
      ]
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
              , div
                  [ style
                      [ ("width", "100%")
                      , ("position", "relative")
                      ]
                  ]
                  [ renderDetailLevel model
                  , renderSubModuleGraph
                      nameMap
                      drvModMap
                      timing
                      (mgs ^. mgsSubgraph)
                      (model ^. modelMainModuleGraph, model ^. modelSubModuleGraph)
                  ]
              ]
        )
  where
    sessionInfo = ss ^. serverSessionInfo
    nameMap = mginfoModuleNameMap $ sessionModuleGraph sessionInfo
    drvModMap = ss ^. serverDriverModuleMap
    timing = ss ^. serverTiming . tsTimingMap
    mgs = ss ^. serverModuleGraphState
    clustering = mgs ^. mgsClustering
