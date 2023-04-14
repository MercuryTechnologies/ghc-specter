module GHCSpecter.Render.Components.Tab (
  TabConfig (..),
  compileTab,
) where

import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.Graphics.DSL (
  Color (..),
  Primitive (..),
  Scene (..),
  TextFontFace (..),
  TextPosition (..),
  ViewPort (..),
 )

data TabConfig a = TabConfig
  { tabCfgId :: Text
  , tabCfgSpacing :: Double
  , tabCfgWidth :: Double
  , tabCfgHeight :: Double
  , tabCfgItems :: [(a, Text)]
  }

compileTab :: (Eq a, Show a) => TabConfig a -> a -> Scene
compileTab cfg tab =
  Scene
    { sceneId = tabCfgId cfg
    , sceneGlobalViewPort = vp
    , sceneLocalViewPort = vp
    , sceneElements = rexp
    }
  where
    spacing = tabCfgSpacing cfg
    height = tabCfgHeight cfg
    items = zip [0 ..] (tabCfgItems cfg)
    mselected = L.find (\(_, (t', _)) -> tab == t') items

    tabPos n = 5 + spacing * n
    end = tabCfgWidth cfg
    vp = ViewPort (0, 0) (end, height)
    fontSize = 8
    mkTab (n, (t, txt)) =
      let x = tabPos n
       in [ Rectangle (x, 2) 80 (height - 2) Nothing (Just White) Nothing (Just (T.pack (show t)))
          , DrawText (x, 2) UpperLeft Sans Black fontSize txt
          ]
    mkLine (Just (n, _)) =
      Polyline
        (0, height)
        [ (tabPos n - 2, height)
        , (tabPos n - 2, 1)
        , (tabPos n - 2 + spacing, 1)
        , (tabPos n - 2 + spacing, height)
        ]
        (end, height)
        Black
        1.0
    mkLine Nothing = Polyline (0, height) [] (end, height) Black 1.0
    rexp =
      concat $
        fmap mkTab items ++ [[mkLine mselected]]
