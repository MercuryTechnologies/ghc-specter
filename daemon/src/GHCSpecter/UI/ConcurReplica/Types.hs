{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHCSpecter.UI.ConcurReplica.Types
  ( -- * IHTML type
    IHTML (..),

    -- * block
    blockDOMUpdate,
  )
where

import Concur.Core
  ( SuspendF (..),
    Widget (Widget, step),
    awaitViewAction,
    mapView,
  )
import Control.Monad.Free (Free (..), hoistFree)
import Control.ShiftMap (ShiftMap (..))
import Replica.VDOM (HTML)

-- | IHTML has additional tag about whether one wants to bypass DOM update.
-- For example, onMouseMove events are fired too frequently, and most of the handling action
-- is just to update internal state, not leading to DOM changes.
-- With IHTML, we tag the HTML content as non-update and bypass expensive websocket diff update steps.
-- Left: no need for update, Right: need for update
data IHTML
  = NoUpdate
  | Update HTML

instance Semigroup IHTML where
  NoUpdate <> NoUpdate = NoUpdate
  NoUpdate <> Update e2 = Update e2
  Update e1 <> NoUpdate = Update e1
  Update e1 <> Update e2 = Update (e1 <> e2)

instance Monoid IHTML where
  mempty = NoUpdate

-- instance ShiftMap (Widget IHTML) (Widget IHTML) where
--   shiftMap f = f

{-
instance ShiftMap (Widget HTML) (Widget IHTML) where
  -- shiftMap ::
  --    (forall a. Widget HTML a -> Widget HTML a) ->
  --    (forall b. Widget IHTML b -> Widget IHTML b)
  shiftMap f t =
    let f' = step . f . Widget

        -- stepT :: Free (SuspendF IHTML) a
        stepT = step t

        convert :: Free (SuspendF IHTML) a -> Free (SuspendF IHTML) a
        convert s =
          case s of
            Pure r -> Pure r
            Free (StepView NoUpdate next) -> Free (StepView NoUpdate next)
            Free (StepView (Update v) next) ->
              let stepS = Free (StepView v (Pure ()))
                  stepS' = f' stepS
               in case stepS' of
                    -- NOTE (IWK): This seems very ad hoc, but I couldn't find
                    -- any better solution yet
                    Free (StepView v' _) -> Free (StepView (Update v') (convert next))
                    _ -> Free Forever
            Free (StepBlock a next) -> Free (StepBlock a next)
            Free (StepSTM a next) -> Free (StepSTM a next)
            Free (StepIO a next) -> Free (StepIO a next)
            Free Forever -> Free Forever

        stepT' = convert stepT
     in Widget stepT'
-}

blockDOMUpdate :: Widget IHTML a
blockDOMUpdate = awaitViewAction (\_ -> NoUpdate)
