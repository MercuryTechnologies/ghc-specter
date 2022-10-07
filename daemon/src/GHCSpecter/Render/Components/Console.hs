{-# LANGUAGE LambdaCase #-}

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
import Data.Text qualified as T
import Data.Tree (Tree (..), drawTree, foldTree)
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

data Expr
  = Bind Text Expr
  | --   | Let
    Other (Text, Text) [Expr]
  deriving (Show)

toExpr :: Tree (Text, Text) -> Either Text Expr
toExpr (Node x []) = Right (Other x [])
toExpr (Node x xs) = do
  let (typ, val) = x
  if typ == "Bind"
    then do
      case xs of
        Node (vtyp, vval) [] : e@(Node (etyp, _) _) : [] -> do
          if (vtyp == "Var" && etyp == "Expr")
            then Bind vval <$> toExpr e
            else Left "Bind, not (Var, Expr)"
        _ -> Left "Bind, not 2 children"
    else Other x <$> traverse toExpr xs

renderExpr :: Expr -> Widget IHTML a
renderExpr tr = go tr
  where
    go (Bind var exp) =
      let content = pre [] [text var]
       in divClass "core-expr" [] [content, go exp]
    go (Other (typ, val) ys) =
      let content = pre [] [text (T.pack (show (typ, val)))]
       in divClass "core-expr" [] (content : fmap go ys)

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
    [ div [style [("width", "10px")]] [text "<"]
    , div
        []
        renderedForest
    ]
  where
    -- for now, show first 3 items
    forest' = take 3 forest
    renderErr err = div [] [pre [] [text err]]
    renderedForest =
      fmap ((\case Left err -> renderErr err; Right e -> renderExpr e) . toExpr) forest'

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
