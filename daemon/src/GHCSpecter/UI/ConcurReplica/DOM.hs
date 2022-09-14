module GHCSpecter.UI.ConcurReplica.DOM
  ( text,
  )
where

import Concur.Core (Widget, display)
import Data.Text (Text)
import GHCSpecter.UI.ConcurReplica.Types (IHTML (..))
import Replica.VDOM (VDOM (..))

text :: Text -> Widget IHTML a
text txt = display (IHTML [VText txt])
