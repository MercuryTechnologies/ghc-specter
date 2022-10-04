module GHCSpecter.Render.Components.Console
  ( render,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( Props,
    classList,
    onClick,
    onInput,
    onKeyPress,
    style,
    textProp,
  )
import Concur.Replica.DOM.Events qualified as DE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.UI.ConcurReplica.DOM
  ( div,
    el,
    input,
    nav,
    pre,
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
  [k] ->
  KeyMap k Text ->
  Maybe k ->
  Text ->
  Widget IHTML (ConsoleEvent k)
render tabs contents mfocus inputEntry = div [] [consoleTabs, console]
  where
    divClass :: Text -> [Props a] -> [Widget IHTML a] -> Widget IHTML a
    divClass cls props = div (classList [(cls, True)] : props)
    navbarMenu = divClass "navbar-menu" []
    navbarStart = divClass "navbar-start" []
    navItem k =
      let isActive = Just k == mfocus
          clss
            | isActive = ["navbar-item", "is-tab", "is-active"]
            | otherwise = ["navbar-item", "is-tab"]
          cls = classList $ map (\tag -> (tag, True)) clss
       in el
            "a"
            [cls, ConsoleTab k <$ onClick]
            [text (T.pack (show (fromKey k)))]
    consoleTabs =
      nav
        [classList [("navbar m-0 p-0", True)]]
        [navbarMenu [navbarStart (fmap navItem tabs)]]
    consoleContent =
      let mtxt = do
            focus <- mfocus
            lookupKey focus contents
       in pre
            [ style
                [ ("height", "200px")
                , ("overflow", "scroll")
                ]
            ]
            [text (fromMaybe "" mtxt)]
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
    console =
      divClass
        "console"
        []
        [consoleContent, consoleInput]
