module GHCSpecter.Render.Util
  ( xmlns,
    divClass,
    cssLink,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( Props,
    classList,
    textProp,
  )
import Data.Text (Text)
import GHCSpecter.UI.ConcurReplica.DOM
  ( div,
    link,
  )
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import Prelude hiding (div)

xmlns :: Props a
xmlns = textProp "xmlns" "http://www.w3.org/2000/svg"

divClass :: Text -> [Props a] -> [Widget IHTML a] -> Widget IHTML a
divClass cls props = div (classList [(cls, True)] : props)

cssLink :: Text -> Widget IHTML a
cssLink url =
  link
    [ textProp "rel" "stylesheet"
    , textProp "href" url
    ]
