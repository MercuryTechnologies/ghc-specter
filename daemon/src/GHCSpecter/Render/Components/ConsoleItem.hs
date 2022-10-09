module GHCSpecter.Render.Components.ConsoleItem
  ( render,
  )
where

import Concur.Core (Widget)
import Concur.Replica (onClick, style)
import Data.Text qualified as T
import GHCSpecter.Data.GHC.Core (toBind)
import GHCSpecter.Render.Components.GHCCore (renderTopBind)
import GHCSpecter.Render.Util (divClass)
import GHCSpecter.Server.Types (ConsoleItem (..))
import GHCSpecter.UI.ConcurReplica.DOM
  ( button,
    div,
    pre,
    text,
  )
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Types.Event (ConsoleEvent (..))
import Prelude hiding (div)

render :: ConsoleItem -> Widget IHTML (ConsoleEvent k)
render (ConsoleText txt) =
  divClass
    "console-item"
    []
    [ div [style [("width", "10px")]] [text "<"]
    , pre [] [text txt]
    ]
render (ConsoleButton buttonss) =
  let mkButton (label, cmd) =
        button
          [ ConsoleButtonPressed cmd <$ onClick
          , style [("display", "inline-block")]
          ]
          [text label]
      mkRow buttons =
        div [style [("display", "block")]] (fmap mkButton buttons)
      rows = fmap mkRow buttonss
   in divClass
        "console-item"
        []
        (div [style [("width", "10px")]] [text "<"] : rows)
render (ConsoleCore forest) =
  divClass
    "console-item"
    []
    (divClass "langle" [] [text "<"] : renderedForest)
  where
    renderErr err = divClass "error" [] [pre [] [text err]]
    render1 tr =
      let -- for debug
          -- txt = T.pack $ drawTree $ fmap show tr
          ebind = toBind tr
          rendered =
            case ebind of
              Left err -> renderErr err
              Right bind -> renderTopBind bind
       in div
            [style [("display", "block"), ("margin", "0"), ("padding", "0")]]
            [ -- for debug
              -- divClass "noinline" [] [pre [] [text txt]]
              div [style [("display", "block")]] [rendered]
            ]
    renderedForest = fmap render1 forest
