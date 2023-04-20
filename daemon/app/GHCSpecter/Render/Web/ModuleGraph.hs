{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GHCSpecter.Render.Web.ModuleGraph (
  -- * Render HTML for the Module Graph tab
  render,
) where

import Concur.Core (Widget)
import Concur.Replica (
  classList,
  onInput,
  style,
 )
import Concur.Replica.DOM.Props qualified as DP (checked, name, type_)
import Control.Error.Util (note)
import Control.Lens (to, (^.), _1)
import Data.IntMap (IntMap)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.Channel.Common.Types (DriverId, type ModuleName)
import GHCSpecter.Channel.Outbound.Types (
  ModuleGraphInfo (..),
  SessionInfo (..),
  Timer,
 )
import GHCSpecter.Data.Map (BiKeyMap, KeyMap)
import GHCSpecter.Data.Timing.Util (isModuleCompilationDone)
import GHCSpecter.GraphLayout.Types (GraphVisInfo (..))
import GHCSpecter.Render.Components.GraphView qualified as GraphView
import GHCSpecter.Server.Types (
  HasModuleGraphState (..),
  HasServerState (..),
  HasTimingState (..),
  ServerState (..),
 )
import GHCSpecter.UI.ConcurReplica.DOM (
  div,
  input,
  label,
  pre,
  text,
 )
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Constants (widgetHeight)
import GHCSpecter.UI.Types (
  HasModuleGraphUI (..),
  HasUIModel (..),
  ModuleGraphUI (..),
  UIModel,
 )
import GHCSpecter.UI.Types.Event (
  DetailLevel (..),
  Event (..),
  SubModuleEvent (..),
 )
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
            valueFor
            grVisInfo
            (mclicked, mhovered)
      ]
    where
      mclicked = mgUI ^. modGraphUIClick
      mhovered = mgUI ^. modGraphUIHover
      valueFor name =
        fromMaybe 0 $ do
          cluster <- L.lookup name clustering
          let nTot = length cluster
          if nTot == 0
            then Nothing
            else do
              let compiled = filter (isModuleCompilationDone drvModMap timing) cluster
                  nCompiled = length compiled
              pure (fromIntegral nCompiled / fromIntegral nTot)

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
            let valueFor name
                  | isModuleCompilationDone drvModMap timing name = 1
                  | otherwise = 0
             in div
                  [ classList [("box", True)]
                  , style [("overflow", "scroll")]
                  ]
                  [ SubModuleEv . SubModuleGraphEv
                      <$> GraphView.renderModuleGraph
                        nameMap
                        valueFor
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
    nameMap = ss ^. serverModuleGraphState . mgsModuleGraphInfo . to mginfoModuleNameMap
    drvModMap = ss ^. serverDriverModuleMap
    timing = ss ^. serverTiming . tsTimingMap
    mgs = ss ^. serverModuleGraphState
    clustering = mgs ^. mgsClustering
