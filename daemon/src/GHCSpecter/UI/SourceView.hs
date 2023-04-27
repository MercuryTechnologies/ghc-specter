{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.UI.SourceView (
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
import GHCSpecter.Layouter.Graph.Types (
  Dimension (..),
  HasGraphVisInfo (..),
 )
import GHCSpecter.Server.Types (
  HasServerState (..),
  ServerState (..),
  SupplementaryView (..),
 )
import GHCSpecter.UI.Components.GraphView qualified as GraphView
import GHCSpecter.UI.Components.Tab qualified as Tab
import GHCSpecter.UI.Components.TextView qualified as TextView
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
    , sceneGlobalViewPort = extent
    , sceneLocalViewPort = extent
    , sceneElements =
        fmap (() <$) $ GraphView.buildGraph (isJust . T.find (== '.')) grVis
    , sceneExtent = Just extent
    }
  where
    Dim canvasWidth canvasHeight = grVis ^. gviCanvasDim
    extent = ViewPort (0, 0) (canvasWidth + 100, canvasHeight + 100)
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