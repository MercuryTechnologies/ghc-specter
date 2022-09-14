module GHCSpecter.UI.ConcurReplica.DOM.Events
  ( onMouseMove,
  )
where

import Concur.Replica.DOM.Props (Prop (PropEvent), Props (Props))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Scientific (toRealFloat)
import Replica.VDOM.Types (DOMEvent (getDOMEvent))

-- | MouseEvent has a bug since "value" can be missing
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
