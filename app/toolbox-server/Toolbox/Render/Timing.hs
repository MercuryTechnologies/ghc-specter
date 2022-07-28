module Toolbox.Render.Timing
  ( renderTiming,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( Props,
    height,
    textProp,
    width,
  )
import qualified Concur.Replica.SVG as S
import qualified Concur.Replica.SVG.Props as SP
import Replica.VDOM.Types (HTML)

xmlns :: Props a
xmlns = textProp "xmlns" "http://www.w3.org/2000/svg"

renderTiming :: Widget HTML a
renderTiming =
  S.svg
    [width "200", height "200", SP.version "1.1", xmlns]
    [ S.rect
        [SP.x "60", SP.y "60", width "80", height "80", SP.fill "red"]
        []
    ]
