{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHCSpecter.UI.ConcurReplica.Types
  ( IHTML (..),
  )
where

import Concur.Core (SuspendF (..), Widget (Widget, step))
import Control.Monad.Free (hoistFree)
import Control.ShiftMap (ShiftMap (..))
import Replica.VDOM (HTML)

newtype IHTML = IHTML {unIHTML :: HTML}
  deriving (Semigroup, Monoid)

instance ShiftMap (Widget HTML) (Widget IHTML) where
  shiftMap f t =
    let -- stepT :: Free (SuspendF IHTML) a
        stepT = step t

        to :: SuspendF IHTML a -> SuspendF HTML a
        to (StepView v next) = StepView (unIHTML v) next
        to (StepBlock a next) = StepBlock a next
        to (StepSTM a next) = StepSTM a next
        to (StepIO a next) = StepIO a next
        to Forever = Forever

        fro :: SuspendF HTML a -> SuspendF IHTML a
        fro (StepView v next) = StepView (IHTML v) next
        fro (StepBlock a next) = StepBlock a next
        fro (StepSTM a next) = StepSTM a next
        fro (StepIO a next) = StepIO a next
        fro Forever = Forever

        -- stepS :: Free (SuspendF HTML) a
        stepS = hoistFree to stepT
        stepS' = step (f (Widget stepS))
        stepT' = hoistFree fro stepS'
     in Widget stepT'
