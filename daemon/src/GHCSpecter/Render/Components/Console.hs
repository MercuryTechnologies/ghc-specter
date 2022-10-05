module GHCSpecter.Render.Components.Console
  ( render,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( classList,
    id,
    onClick,
    onInput,
    onKeyPress,
    style,
    textProp,
  )
import Concur.Replica.DOM.Events qualified as DE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHCSpecter.Render.Util (divClass)
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
import Prelude hiding (div, id)

render ::
  (IsKey k, Eq k) =>
  [(k, Text)] ->
  KeyMap k Text ->
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
      let mtxt = do
            focus <- mfocus
            lookupKey focus contents

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
       in pre
            [ id "console"
            , style
                [ ("height", "200px")
                , ("overflow", "scroll")
                ]
            ]
            [ scriptContent
            , text (fromMaybe "" mtxt)
            ]
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
