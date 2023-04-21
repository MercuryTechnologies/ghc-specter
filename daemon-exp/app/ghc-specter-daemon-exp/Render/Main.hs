{-# LANGUAGE OverloadedStrings #-}

module Render.Main (
  renderNotConnected,
  renderAction,
) where

import Control.Lens (to, (^.))
import Control.Monad.Trans.Class (lift)
import GHCSpecter.Channel.Outbound.Types (ModuleGraphInfo (..))
import GHCSpecter.Graphics.DSL (
  Color (Black),
  TextFontFace (Sans),
 )
import GHCSpecter.Server.Types (
  HasModuleGraphState (..),
  HasServerState (..),
  HasTimingState (..),
  ServerState (..),
 )
import GHCSpecter.UI.Types (
  HasUIModel (..),
  HasUIState (..),
  UIState,
 )
import GHCSpecter.UI.Types.Event (
  Event (..),
  Tab (..),
 )
import GI.Cairo.Render qualified as R
import Render.Parts.ModuleGraph (renderModuleGraph)
import Render.Parts.Session (renderSession)
import Render.Parts.SourceView (renderSourceView)
import Render.Parts.Timing (renderTiming)
import Renderer (drawText, setColor)
import Types (
  GtkRender,
 )

renderNotConnected :: GtkRender e ()
renderNotConnected = do
  lift R.save
  setColor Black
  drawText Sans 36 (100, 100) "GHC is not connected yet"
  lift R.restore

renderAction ::
  UIState ->
  ServerState ->
  GtkRender Event ()
renderAction ui ss = do
  let nameMap =
        ss ^. serverModuleGraphState . mgsModuleGraphInfo . to mginfoModuleNameMap
      drvModMap = ss ^. serverDriverModuleMap
      timing = ss ^. serverTiming . tsTimingMap
      mgs = ss ^. serverModuleGraphState
      clustering = mgs ^. mgsClustering
      mgrvis = mgs ^. mgsClusterGraph
      mgrui = ui ^. uiModel . modelMainModuleGraph
      sgrui = ui ^. uiModel . modelSubModuleGraph
      subgraphs = mgs ^. mgsSubgraph
      sessui = ui ^. uiModel . modelSession

  case mgrvis of
    Nothing -> renderNotConnected
    Just grVisInfo ->
      case ui ^. uiModel . modelTab of
        TabSession ->
          renderSession
            ss
            sessui
        TabModuleGraph ->
          renderModuleGraph
            (mgrui, sgrui)
            subgraphs
            nameMap
            drvModMap
            timing
            clustering
            grVisInfo
        TabSourceView -> do
          let srcUI = ui ^. uiModel . modelSourceView
          renderSourceView srcUI ss
        TabTiming -> do
          let tui = ui ^. uiModel . modelTiming
              ttable = ss ^. serverTiming . tsTimingTable
          renderTiming drvModMap tui ttable
