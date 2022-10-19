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
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import GHCSpecter.Render.Components.ConsoleItem qualified as CI (render)
import GHCSpecter.Render.Util (divClass)
import GHCSpecter.Server.Types (ConsoleItem (..))
import GHCSpecter.UI.ConcurReplica.DOM
  ( button,
    div,
    el,
    input,
    nav,
    p,
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

render ::
  (IsKey k, Eq k) =>
  [(k, Text)] ->
  KeyMap k [ConsoleItem] ->
  -- | getHelp. (title, help items), help item: Left: button, Right: text
  (k -> (Text, [Either (Text, ConsoleEvent k) Text])) ->
  Maybe k ->
  Text ->
  Widget IHTML (ConsoleEvent k)
render tabs contents getHelp mfocus inputEntry = div [] [consoleTabs, console]
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
                  \var config = {attributes: true, childList: true, subtree: true, characterData: true };\n\
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
            (scriptContent : fmap CI.render (join (maybeToList mtxts)))
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
    consoleHelp =
      let mhelp = getHelp <$> mfocus
          (title, items) = fromMaybe ("", []) mhelp
          titleElem =
            divClass "console-help-title" [] [text title]
          renderItem (Left (txt, ev)) = div [] [button [ev <$ onClick] [text txt]]
          renderItem (Right txt) = p [] [text txt]
          helpElems =
            divClass "console-help-content" [] (fmap renderItem items)
       in divClass "console-help" [] [titleElem, helpElems]
    console = divClass "console" [] [consoleContent, consoleInput, consoleHelp]
