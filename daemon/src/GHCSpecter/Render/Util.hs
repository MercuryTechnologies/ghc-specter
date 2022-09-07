module GHCSpecter.Render.Util
  ( xmlns,
  )
where

import Concur.Replica
  ( Props,
    textProp,
  )

xmlns :: Props a
xmlns = textProp "xmlns" "http://www.w3.org/2000/svg"
