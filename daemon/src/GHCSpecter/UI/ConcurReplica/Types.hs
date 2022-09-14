{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHCSpecter.UI.ConcurReplica.Types
  ( -- * IHTML type
    IHTML (..),

    -- * IHTML <-> HTML
    project,
    embed,

    -- * block / unblock DOM
    blockDOMUpdate,
    unblockDOMUpdate,
  )
where

import Concur.Core
  ( SuspendF (..),
    Widget (Widget, step),
    mapView,
  )
import Control.Monad.Free (hoistFree)
import Control.ShiftMap (ShiftMap (..))
import Replica.VDOM (HTML)

-- | Left: no need for update, Right: need for update
newtype IHTML = IHTML {unIHTML :: Either HTML HTML}

instance Semigroup IHTML where
  IHTML (Left e1) <> IHTML (Left e2) = IHTML (Left (e1 <> e2))
  IHTML (Left e1) <> IHTML (Right e2) = IHTML (Right (e1 <> e2))
  IHTML (Right e1) <> IHTML (Left e2) = IHTML (Right (e1 <> e2))
  IHTML (Right e1) <> IHTML (Right e2) = IHTML (Right (e1 <> e2))

instance Monoid IHTML where
  mempty = IHTML (Left mempty)

project :: IHTML -> HTML
project (IHTML (Left a)) = a
project (IHTML (Right a)) = a

embed :: HTML -> IHTML
embed a = IHTML (Right a)

instance ShiftMap (Widget HTML) (Widget IHTML) where
  shiftMap f t =
    let -- stepT :: Free (SuspendF IHTML) a
        stepT = step t

        to :: SuspendF IHTML a -> SuspendF HTML a
        to (StepView v next) = StepView (project v) next
        to (StepBlock a next) = StepBlock a next
        to (StepSTM a next) = StepSTM a next
        to (StepIO a next) = StepIO a next
        to Forever = Forever

        fro :: SuspendF HTML a -> SuspendF IHTML a
        fro (StepView v next) = StepView (embed v) next
        fro (StepBlock a next) = StepBlock a next
        fro (StepSTM a next) = StepSTM a next
        fro (StepIO a next) = StepIO a next
        fro Forever = Forever

        -- stepS :: Free (SuspendF HTML) a
        stepS = hoistFree to stepT
        stepS' = step (f (Widget stepS))
        stepT' = hoistFree fro stepS'
     in Widget stepT'

blockDOMUpdate :: Widget IHTML a -> Widget IHTML a
blockDOMUpdate = mapView (\x -> IHTML (Left (project x)))

unblockDOMUpdate :: Widget IHTML a -> Widget IHTML a
unblockDOMUpdate = mapView (\x -> IHTML (Right (project x)))
