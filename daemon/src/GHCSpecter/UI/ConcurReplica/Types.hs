{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHCSpecter.UI.ConcurReplica.Types (
  -- * IHTML type
  IHTML (..),

  -- * project
  project,

  -- * block
  blockDOMUpdate,
  unblockDOMUpdate,
) where

import Concur.Core (
  Widget,
  mapView,
 )
import Replica.VDOM (HTML)

-- | IHTML has additional tag about whether one wants to bypass DOM update.
-- For example, onMouseMove events are fired too frequently, and most of the handling action
-- is just to update internal state, not leading to DOM changes.
-- With IHTML, we tag the HTML content as non-update and bypass expensive websocket diff update steps.
data IHTML
  = -- | update
    Update HTML
  | -- | no update
    NoUpdate HTML

-- NOTE: NoUpdate is the upper bound of IHTML and Update [] is mempty, so once meeting NoUpate,
-- the DOM is not updated after that.
instance Semigroup IHTML where
  Update e1 <> Update e2 = Update (e1 <> e2)
  Update e1 <> NoUpdate e2 = NoUpdate (e1 <> e2)
  NoUpdate e1 <> Update e2 = NoUpdate (e1 <> e2)
  NoUpdate e1 <> NoUpdate e2 = NoUpdate (e1 <> e2)

instance Monoid IHTML where
  mempty = Update []

project :: IHTML -> HTML
project (NoUpdate v) = v
project (Update v) = v

blockDOMUpdate :: Widget IHTML a -> Widget IHTML a
blockDOMUpdate = mapView (\x -> NoUpdate (project x))

unblockDOMUpdate :: Widget IHTML a -> Widget IHTML a
unblockDOMUpdate = mapView (\x -> Update (project x))
