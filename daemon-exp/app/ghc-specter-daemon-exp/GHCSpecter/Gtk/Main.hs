{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Gtk.Main (
  renderNotConnected,
  renderAction,
) where

import Control.Concurrent.STM (
  atomically,
  modifyTVar',
 )
import Control.Lens (to, (^.))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Data.Foldable (for_)
import Data.Map qualified as Map
import GHCSpecter.Channel.Outbound.Types (
  ModuleGraphInfo (..),
  SessionInfo (..),
 )
import GHCSpecter.Graphics.DSL (
  Color (..),
  Scene (..),
  TextFontFace (Sans),
 )
import GHCSpecter.Gtk.Console (renderConsole)
import GHCSpecter.Gtk.ModuleGraph (renderModuleGraph)
import GHCSpecter.Gtk.Renderer (
  drawText,
  render,
  setColor,
 )
import GHCSpecter.Gtk.Session (renderSession)
import GHCSpecter.Gtk.SourceView (renderSourceView)
import GHCSpecter.Gtk.Timing (renderTiming)
import GHCSpecter.Gtk.Types (
  GtkRender,
  ViewBackend (..),
 )
import GHCSpecter.Server.Types (
  HasModuleGraphState (..),
  HasServerState (..),
  HasTimingState (..),
  ServerState (..),
 )
import GHCSpecter.UI.Components.Tab (buildTab)
import GHCSpecter.UI.Constants (HasWidgetConfig (..))
import GHCSpecter.UI.Tab (topLevelTab)
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

resetWidget :: GtkRender e ()
resetWidget = do
  vb <- ask
  let emapRef = vbEventMap vb
  liftIO $ atomically $ do
    modifyTVar' emapRef (const [])

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
  case mgrvis of
    Nothing -> renderNotConnected
    Just grVisInfo -> do
      resetWidget
      wcfg <- (^. to vbWidgetConfig . wcfgTopLevel) <$> ask
      -- tab
      for_ (Map.lookup "tab" wcfg) $ \vpCvs -> do
        sceneTab <- fmap (fmap TabEv) <$> buildTab topLevelTab (Just (ui ^. uiModel . modelTab))
        let sceneTab' =
              sceneTab
                { sceneGlobalViewPort = vpCvs
                }
        render sceneTab'
      -- main
      case ui ^. uiModel . modelTab of
        TabSession -> renderSession ss
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
      -- console
      when (ss ^. serverSessionInfo . to sessionIsPaused) $ do
        renderConsole ui ss
