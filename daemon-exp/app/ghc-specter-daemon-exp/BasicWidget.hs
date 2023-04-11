{-# LANGUAGE OverloadedStrings #-}

module BasicWidget (
  compileTab,
) where

import Control.Lens ((^.), _1)
import GHCSpecter.Graphics.DSL (
  Color (..),
  Primitive (..),
  Scene (..),
  TextPosition (..),
  ViewPort (..),
 )
import GHCSpecter.UI.Constants (canvasDim, tabHeight)
import GHCSpecter.UI.Types.Event (Tab (..))

compileTab :: Tab -> Scene
compileTab tab =
  Scene
    { sceneId = "tab"
    , sceneGlobalViewPort = vp
    , sceneLocalViewPort = vp
    , sceneElements = rexp
    }
  where
    tabPos TabSession = 5
    tabPos TabModuleGraph = 85
    tabPos TabSourceView = 165
    tabPos TabTiming = 245
    end = canvasDim ^. _1
    vp = ViewPort (0, 0) (end, tabHeight)
    fontSize = 8
    rexp =
      [ DrawText (tabPos TabSession, 2) UpperLeft Black fontSize "Session"
      , DrawText (tabPos TabModuleGraph, 2) UpperLeft Black fontSize "Module Graph"
      , DrawText (tabPos TabSourceView, 2) UpperLeft Black fontSize "Source View"
      , DrawText (tabPos TabTiming, 2) UpperLeft Black fontSize "Timing"
      , Polyline
          (0, tabHeight)
          [ (tabPos tab - 2, tabHeight)
          , (tabPos tab - 2, 1)
          , (tabPos tab + 78, 1)
          , (tabPos tab + 78, tabHeight)
          ]
          (end, tabHeight)
          Black
          1.0
      ]
