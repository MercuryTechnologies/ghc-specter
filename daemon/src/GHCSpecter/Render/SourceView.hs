{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Render.SourceView (
  compileSuppViewPanel,
) where

import Control.Lens ((^.))
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import GHCSpecter.Channel.Common.Types (ModuleName)
import GHCSpecter.Graphics.DSL (Scene (..), ViewPort (..))
import GHCSpecter.Render.Components.GraphView qualified as GraphView
import GHCSpecter.Render.Components.Tab qualified as Tab
import GHCSpecter.Server.Types (
  HasServerState (..),
  ServerState (..),
  SupplementaryView (..),
 )
import GHCSpecter.UI.Constants (canvasDim)
import GHCSpecter.UI.Types (
  HasSourceViewUI (..),
  SourceViewUI (..),
 )
import GHCSpecter.UI.Types.Event (
  SourceViewEvent (..),
 )

compileSuppView :: Maybe SupplementaryView -> Scene ()
compileSuppView Nothing =
  Scene
    { sceneId = "supple-view-contents"
    , sceneGlobalViewPort = ViewPort (0, 0) canvasDim
    , sceneLocalViewPort = ViewPort (0, 0) canvasDim
    , sceneElements = []
    }
compileSuppView (Just (SuppViewCallgraph grVis)) =
  Scene
    { sceneId = "supple-view-contents"
    , sceneGlobalViewPort = ViewPort (0, 0) canvasDim
    , sceneLocalViewPort = ViewPort (0, 0) canvasDim
    , sceneElements =
        fmap (() <$) $ GraphView.compileGraph (isJust . T.find (== '.')) grVis
    }
compileSuppView (Just (SuppViewText _)) =
  Scene
    { sceneId = "supple-view-contents"
    , sceneGlobalViewPort = ViewPort (0, 0) canvasDim
    , sceneLocalViewPort = ViewPort (0, 0) canvasDim
    , sceneElements = []
    }

compileSuppViewPanel ::
  ModuleName ->
  SourceViewUI ->
  ServerState ->
  (Scene SourceViewEvent, Scene ())
compileSuppViewPanel modu srcUI ss =
  ( fmap SourceViewTab (Tab.compileTab tabCfg mtab)
  , compileSuppView msuppView
  )
  where
    mtab = srcUI ^. srcViewSuppViewTab
    suppViews = fromMaybe [] (M.lookup modu (ss ^. serverSuppView))
    msuppView = do
      tab <- mtab
      suppView <- L.lookup tab suppViews
      pure suppView
    suppViewTabs = fmap (\((t, i), _) -> ((t, i), t <> ":" <> T.pack (show i))) suppViews
    tabCfg =
      Tab.TabConfig
        { Tab.tabCfgId = "supple-view-tab"
        , Tab.tabCfgSpacing = 80
        , Tab.tabCfgWidth = 500
        , Tab.tabCfgHeight = 15
        , Tab.tabCfgItems = suppViewTabs
        }
