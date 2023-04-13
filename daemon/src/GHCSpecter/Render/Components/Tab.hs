{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Render.Components.Tab (
  compileTab,
) where

import Control.Lens ((^.), _1)
import Data.Text qualified as T
import GHCSpecter.Graphics.DSL (
  Color (..),
  Primitive (..),
  Scene (..),
  TextFontFace (..),
  TextPosition (..),
  ViewPort (..),
 )
import GHCSpecter.UI.Constants (canvasDim, tabHeight)
import GHCSpecter.UI.Types.Event (Tab (..))

-- TODO: make this generic
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
    mkTab t txt =
      let x = tabPos t
       in [ Rectangle (x, 2) 80 (tabHeight - 2) Nothing (Just White) Nothing (Just (T.pack (show t)))
          , DrawText (tabPos t, 2) UpperLeft Sans Black fontSize txt
          ]
    mkLine =
      Polyline
        (0, tabHeight)
        [ (tabPos tab - 2, tabHeight)
        , (tabPos tab - 2, 1)
        , (tabPos tab + 78, 1)
        , (tabPos tab + 78, tabHeight)
        ]
        (end, tabHeight)
        Black
        1.0
    rexp =
      concat $
        [ mkTab TabSession "Session"
        , mkTab TabModuleGraph "Module Graph"
        , mkTab TabSourceView "Source View"
        , mkTab TabTiming "Timing"
        , [mkLine]
        ]
