module GHCSpecter.Render.Components.Console
  ( render,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( classList,
    onClick,
    onInput,
    onKeyPress,
    style,
    textProp,
  )
import Concur.Replica.DOM.Events qualified as DE
import Control.Monad (join)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import GHCSpecter.Data.GHC.Core (toBind)
import GHCSpecter.Render.Components.GHCCore (renderTopBind)
import GHCSpecter.Render.Util (divClass)
import GHCSpecter.Server.Types (ConsoleItem (..))
import GHCSpecter.UI.ConcurReplica.DOM
  ( div,
    el,
    input,
    nav,
    pre,
    script,
    text,
  )
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Types.Event (ConsoleEvent (..))
import GHCSpecter.Util.Map
  ( IsKey (..),
    KeyMap,
    lookupKey,
  )
import Prelude hiding (div)

renderConsoleItem :: ConsoleItem -> Widget IHTML a
renderConsoleItem (ConsoleText txt) =
  divClass
    "console-item"
    []
    [ div [style [("width", "10px")]] [text "<"]
    , pre [] [text txt]
    ]
renderConsoleItem (ConsoleCore forest) =
  divClass
    "console-item"
    []
    (divClass "langle" [] [text "<"] : renderedForest)
  where
    forest' = take 3 forest
    renderErr err = divClass "error" [] [pre [] [text err]]
    render1 tr =
      let -- for debug
          -- txt = T.pack $ drawTree $ fmap show tr
          ebind = toBind tr
          rendered =
            case ebind of
              Left err -> renderErr err
              Right bind -> renderTopBind bind
       in divClass
            "nomargin"
            []
            [ -- for debug
              -- divClass "noinline" [] [pre [] [text txt]],
              divClass "noinline" [] [rendered]
            ]
    renderedForest = fmap render1 forest'

render ::
  (IsKey k, Eq k) =>
  [(k, Text)] ->
  KeyMap k [ConsoleItem] ->
  Maybe k ->
  Text ->
  Widget IHTML (ConsoleEvent k)
render tabs contents mfocus inputEntry = div [] [consoleTabs, console]
  where
    navbarMenu = divClass "navbar-menu" []
    navbarStart = divClass "navbar-start" []
    navItem (k, tab) =
      let isActive = Just k == mfocus
          clss
            | isActive = ["navbar-item", "is-tab", "console-tab", "is-active"]
            | otherwise = ["navbar-item", "is-tab", "console-tab"]
          cls = classList $ map (\tag -> (tag, True)) clss
       in el
            "a"
            [cls, ConsoleTab k <$ onClick]
            [text tab]
    consoleTabs =
      nav
        [classList [("navbar", True)]]
        [navbarMenu [navbarStart (fmap navItem tabs)]]
    consoleContent =
      let mtxts = mfocus >>= (`lookupKey` contents)

          -- This is a hack. Property update should be supported by concur-replica.
          -- TODO: implement prop update in internalized concur-replica.
          scriptContent =
            script
              []
              [ text
                  "var me = document.currentScript;\n\
                  \var myParent = me.parentElement;\n\
                  \console.log(myParent);\n\
                  \var config = {attirbutes: true, childList: true, subtree: true, characterData: true };\n\
                  \var callback = (mutationList, observer) => {\n\
                  \      myParent.scrollTop = myParent.scrollHeight;\n\
                  \    };\n\
                  \var observer = new MutationObserver(callback);\n\
                  \observer.observe(myParent, config);\n"
              ]
       in div
            [ classList [("box", True)]
            , style
                [ ("height", "200px")
                , ("overflow", "scroll")
                ]
            ]
            (scriptContent : fmap renderConsoleItem (join (maybeToList mtxts)))
    consoleInput =
      divClass
        "console-input"
        []
        [ input
            [ ConsoleInput . DE.targetValue . DE.target <$> onInput
            , ConsoleKey . DE.kbdKey <$> onKeyPress
            , classList [("input", True)]
            , textProp "type" "text"
            , textProp "placeholder" "type inspection command"
            , textProp "value" inputEntry
            ]
        ]
    console = divClass "console" [] [consoleContent, consoleInput]
