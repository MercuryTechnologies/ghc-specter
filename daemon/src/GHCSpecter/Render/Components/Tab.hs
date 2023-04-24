module GHCSpecter.Render.Components.Tab (
  TabConfig (..),
  buildTab,
) where

import Data.List qualified as L
import Data.Text (Text)
import GHCSpecter.Graphics.DSL (
  Color (..),
  HitEvent (..),
  Scene (..),
  TextFontFace (..),
  TextPosition (..),
  ViewPort (..),
  drawText,
  polyline,
  rectangle,
 )

data TabConfig tab = TabConfig
  { tabCfgId :: Text
  , tabCfgSpacing :: Double
  , tabCfgWidth :: Double
  , tabCfgHeight :: Double
  , tabCfgItems :: [(tab, Text)]
  }

buildTab :: (Eq tab) => TabConfig tab -> Maybe tab -> Scene tab
buildTab cfg mtab =
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
    mselected = do
      tab <- mtab
      L.find (\(_, (tab', _)) -> tab == tab') items

    tabPos n = 5 + spacing * fromIntegral n
    end = tabCfgWidth cfg
    vp = ViewPort (0, 0) (end, height)
    fontSize = 8
    mkTab (n, (tab, txt)) =
      let x = tabPos (n :: Int)
          hitEvent =
            HitEvent
              { hitEventHoverOn = Nothing
              , hitEventHoverOff = Nothing
              , hitEventClick = Just (Right tab)
              }
       in [ rectangle (x, 2) 80 (height - 2) Nothing (Just White) Nothing (Just hitEvent)
          , drawText (x, 2) UpperLeft Sans Black fontSize txt
          ]
    mkLine (Just (n, _)) =
      polyline
        (0, height)
        [ (tabPos n - 2, height)
        , (tabPos n - 2, 1)
        , (tabPos n - 2 + spacing, 1)
        , (tabPos n - 2 + spacing, height)
        ]
        (end, height)
        Black
        1.0
    mkLine Nothing = polyline (0, height) [] (end, height) Black 1.0
    rexp =
      concat $
        fmap mkTab items ++ [[mkLine mselected]]
