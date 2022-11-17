module GHCSpecter.UI.ConcurReplica.DOM.Events (
  onMouseMove,
  onMouseDown,
  onMouseUp,
) where

import Concur.Replica.DOM.Props (Prop (PropEvent), Props (Props))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Scientific (toRealFloat)
import Replica.VDOM.Types (DOMEvent (getDOMEvent))

-- | @Concur.Replica.DOM.Event.MouseEvent@ has a bug since "value" can be missing
onMouseMove :: Props (Maybe (Double, Double))
onMouseMove = Props "onMouseMove" (PropEvent (getClientXY . getDOMEvent))
  where
    getClientXY (A.Object m) = do
      cX <- A.lookup "clientX" m
      cY <- A.lookup "clientY" m
      case (cX, cY) of
        (A.Number x, A.Number y) -> pure (toRealFloat x, toRealFloat y)
        _ -> Nothing
    getClientXY _ = Nothing

-- | @Concur.Replica.DOM.Event.MouseEvent@ has a bug since "value" can be missing
onMouseDown :: Props (Maybe (Double, Double))
onMouseDown = Props "onMouseDown" (PropEvent (getClientXY . getDOMEvent))
  where
    getClientXY (A.Object m) = do
      cX <- A.lookup "clientX" m
      cY <- A.lookup "clientY" m
      case (cX, cY) of
        (A.Number x, A.Number y) -> pure (toRealFloat x, toRealFloat y)
        _ -> Nothing
    getClientXY _ = Nothing

-- | @Concur.Replica.DOM.Event.MouseEvent@ has a bug since "value" can be missing
onMouseUp :: Props (Maybe (Double, Double))
onMouseUp = Props "onMouseUp" (PropEvent (getClientXY . getDOMEvent))
  where
    getClientXY (A.Object m) = do
      cX <- A.lookup "clientX" m
      cY <- A.lookup "clientY" m
      case (cX, cY) of
        (A.Number x, A.Number y) -> pure (toRealFloat x, toRealFloat y)
        _ -> Nothing
    getClientXY _ = Nothing
