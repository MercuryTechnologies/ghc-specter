module GHCSpecter.UI.Components.Tab (
  TabConfig (..),
  buildTab,
) where

import Data.List qualified as L
import Data.Text (Text)
import GHCSpecter.Graphics.DSL (
  Color (..),
  HitEvent (..),
  Primitive,
  Scene (..),
  TextFontFace (..),
  TextPosition (..),
  ViewPort (..),
  polyline,
  rectangle,
 )
import GHCSpecter.Layouter.Text (
  MonadTextLayout,
  drawText',
 )

data TabConfig tab = TabConfig
  { tabCfgId :: Text
  , tabCfgSpacing :: Double
  , tabCfgWidth :: Double
  , tabCfgHeight :: Double
  , tabCfgItems :: [(tab, Text)]
  }

buildTab ::
  forall m tab.
  (MonadTextLayout m, Eq tab) =>
  TabConfig tab ->
  Maybe tab ->
  m (Scene (Primitive tab))
buildTab cfg mtab = do
  renderedTabItems <- traverse mkTab items
  let rexp = concat (renderedTabItems ++ [[mkLine mselected]])
  pure
    Scene
      { sceneId = tabCfgId cfg
      , sceneGlobalViewPort = vp
      , sceneLocalViewPort = vp
      , sceneElements = rexp
      , sceneExtent = Nothing
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
    mkTab (n, (tab, txt)) = do
      let x = tabPos (n :: Int)
          hitEvent =
            HitEvent
              { hitEventHoverOn = Nothing
              , hitEventHoverOff = Nothing
              , hitEventClick = Just (Right tab)
              }
      renderedText <- drawText' (x, 2) UpperLeft Sans Black fontSize txt
      pure
        [ rectangle (x, 2) 80 (height - 2) Nothing (Just White) Nothing (Just hitEvent)
        , renderedText
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
