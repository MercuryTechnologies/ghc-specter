{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Render.SourceView (
  buildSuppViewPanel,
) where

import Control.Lens ((^.))
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import GHCSpecter.Channel.Common.Types (ModuleName)
import GHCSpecter.Graphics.DSL (
  Primitive,
  Scene (..),
  ViewPort (..),
 )
import GHCSpecter.Render.Components.GraphView qualified as GraphView
import GHCSpecter.Render.Components.Tab qualified as Tab
import GHCSpecter.Render.Components.TextView qualified as TextView
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

buildSuppView :: Maybe SupplementaryView -> Scene (Primitive ())
buildSuppView Nothing =
  Scene
    { sceneId = "supple-view-contents"
    , sceneGlobalViewPort = ViewPort (0, 0) canvasDim
    , sceneLocalViewPort = ViewPort (0, 0) canvasDim
    , sceneElements = []
    , sceneExtent = Nothing
    }
buildSuppView (Just (SuppViewCallgraph grVis)) =
  Scene
    { sceneId = "supple-view-contents"
    , sceneGlobalViewPort = ViewPort (0, 0) canvasDim
    , sceneLocalViewPort = ViewPort (0, 0) canvasDim
    , sceneElements =
        fmap (() <$) $ GraphView.buildGraph (isJust . T.find (== '.')) grVis
    , sceneExtent = Nothing
    }
buildSuppView (Just (SuppViewText txt)) =
  let scene = TextView.buildTextView txt []
   in scene
        { sceneId = "supple-view-contents"
        , sceneGlobalViewPort = ViewPort (0, 0) canvasDim
        , sceneLocalViewPort = ViewPort (0, 0) canvasDim
        , sceneExtent = Nothing
        }

buildSuppViewPanel ::
  ModuleName ->
  SourceViewUI ->
  ServerState ->
  (Scene (Primitive SourceViewEvent), Scene (Primitive ()))
buildSuppViewPanel modu srcUI ss =
  ( fmap (fmap SourceViewTab) (Tab.buildTab tabCfg mtab)
  , buildSuppView msuppView
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
