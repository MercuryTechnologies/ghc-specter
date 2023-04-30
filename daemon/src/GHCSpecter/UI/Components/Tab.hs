module GHCSpecter.UI.Components.Tab (
  TabConfig (..),
  buildTab,
) where

import Data.Foldable qualified as F
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (sconcat)
import Data.Text (Text)
import GHCSpecter.Graphics.DSL (
  Color (..),
  HitEvent (..),
  Primitive (..),
  Scene (..),
  TextFontFace (..),
  TextPosition (..),
  ViewPort (..),
  getLeastUpperBoundingBox,
  polyline,
  rectangle,
  viewPortWidth,
 )
import GHCSpecter.Layouter.Packer (flowInline)
import GHCSpecter.Layouter.Text (
  MonadTextLayout,
  drawText',
 )
import Safe (atMay)

data TabConfig tab = TabConfig
  { tabCfgId :: Text
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
  tabItems_ <- traverse mkTab items
  let renderedTabItems =
        case NE.nonEmpty tabItems_ of
          Nothing -> [polyline (0, height) [] (end, height) Black 1.0]
          Just tabItems' ->
            let (_, placed) = flowInline 5 tabItems'
                placed' :: NE.NonEmpty (Primitive tab)
                placed' = sconcat placed
                line = mkLine mselected (F.toList placed)
             in F.toList placed' ++ [line]
      rexp = renderedTabItems
  pure
    Scene
      { sceneId = tabCfgId cfg
      , sceneGlobalViewPort = vp
      , sceneLocalViewPort = vp
      , sceneElements = rexp
      , sceneExtents = Nothing
      }
  where
    height = tabCfgHeight cfg
    items = zip [0 ..] (tabCfgItems cfg)
    mselected = do
      tab <- mtab
      L.find (\(_, (tab', _)) -> tab == tab') items
    end = tabCfgWidth cfg
    vp = ViewPort (0, 0) (end, height)
    fontSize = 8
    mkTab (_, (tab, txt)) = do
      let hitEvent =
            HitEvent
              { hitEventHoverOn = Nothing
              , hitEventHoverOff = Nothing
              , hitEventClick = Just (Right tab)
              }
      renderedText <- drawText' (5, 2) UpperLeft Sans Black fontSize txt
      let bbox = primBoundingBox renderedText
          width = viewPortWidth bbox
      pure $
        rectangle (0, 2) (width + 10) (height - 2) Nothing (Just White) Nothing (Just hitEvent)
          NE.:| [renderedText]
    mkLine (Just (n, _)) tabItems =
      case Safe.atMay tabItems n of
        Nothing -> polyline (0, height) [] (end, height) Black 1.0
        Just e ->
          let ViewPort (x0, _y0) (x1, _y1) = getLeastUpperBoundingBox e
           in polyline
                (0, height)
                [ (x0, height)
                , (x0, 1)
                , (x1, 1)
                , (x1, height)
                ]
                (end, height)
                Black
                1.0
    mkLine Nothing _ = polyline (0, height) [] (end, height) Black 1.0
