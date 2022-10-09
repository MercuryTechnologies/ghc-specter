module GHCSpecter.Render.Components.ConsoleItem
  ( render,
  )
where

import Concur.Core (Widget)
import Concur.Replica (style)
import Data.Text qualified as T
import GHCSpecter.Data.GHC.Core (toBind)
import GHCSpecter.Render.Components.GHCCore (renderTopBind)
import GHCSpecter.Render.Util (divClass)
import GHCSpecter.Server.Types (ConsoleItem (..))
import GHCSpecter.UI.ConcurReplica.DOM
  ( div,
    pre,
    text,
  )
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import Prelude hiding (div)

render :: ConsoleItem -> Widget IHTML a
render (ConsoleText txt) =
  divClass
    "console-item"
    []
    [ div [style [("width", "10px")]] [text "<"]
    , pre [] [text txt]
    ]
render (ConsoleButton buttonss) =
  let txt = T.intercalate "\n" $ fmap (T.intercalate " " . fmap fst) buttonss
   in divClass
        "console-item"
        []
        [ div [style [("width", "10px")]] [text "<"]
        , pre [] [text txt]
        ]
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
