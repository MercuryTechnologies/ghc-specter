module GHCSpecter.UI.ConcurReplica.DOM
  ( text,
  )
where

import Concur.Core (Widget, display)
import Data.Text (Text)
import GHCSpecter.UI.ConcurReplica.Types (IHTML (..), embed)
import Replica.VDOM (VDOM (..))

-- | @Concur.Replica.DOM.text@ was specialized to Widget HTML, so we reintroduced this @text@ for Widget IHTML.
text :: Text -> Widget IHTML a
text txt = display (embed [VText txt])
