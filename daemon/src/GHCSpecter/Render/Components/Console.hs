{-# LANGUAGE MultiWayIf #-}

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

type Var = Text

-- TODO: eventually these will be isomorphic to CoreExpr

data Bind = Bind Var Expr
  deriving (Show)

data Expr
  = -- BindExpr Bind
    Lam Var Expr
  | Let Bind Expr
  | -- | App Expr Expr
    Other (Text, Text) [Expr]
  deriving (Show)

toBind :: Tree (Text, Text) -> Either Text Bind
toBind (Node (typ, val) xs)
  | typ == "Bind" =
      -- TODO: should handle recursive case
      case xs of
        Node (vtyp, vval) [] : e@(Node (etyp, _) _) : [] ->
          if (vtyp == "Var" && etyp == "Expr")
            then Bind vval <$> toExpr e
            else Left "Bind, not (Var, Expr)"
        _ -> Left "Bind, not 2 children"
  | otherwise = Left "not Bind"

toExpr :: Tree (Text, Text) -> Either Text Expr
toExpr x@(Node (typ, val) xs)
  | typ == "Expr" && val == "Lam" = do
      case xs of
        Node (vtyp, vval) [] : e@(Node (etyp, _) _) : [] ->
          if (vtyp == "Var" && etyp == "Expr")
            then Lam vval <$> toExpr e
            else Left "Lam, not (Var, Expr)"
        _ -> Left "Lam, not 2 children"
  | typ == "Expr" && val == "Let" = do
      case xs of
        b : e : [] ->
          Let <$> toBind b <*> toExpr e
        _ -> Left "Lam, not 2 children"
  | otherwise =
      Other (typ, val) <$> traverse toExpr xs

renderTopBind :: Bind -> Widget IHTML a
renderTopBind bind = goB 0 bind
  where
    cls l = if l == 0 then "core-expr top" else "core-expr"

    goB lvl (Bind var exp) =
      let varEl = pre [classList [("core-expr-inline", True)]] [text var]
          eqEl = divClass "core-expr-inline eq" [] [text "="]
          expEl = goE (lvl + 1) exp
       in divClass (cls lvl) [] [varEl, eqEl, expEl]

    goE lvl expr =
      let -- for now, show first 3 items

          -- This is a hack. Property update should be supported by concur-replica.
          -- TODO: implement prop update in internalized concur-replica.

       in case expr of
            Lam var exp ->
              let lambdaEl = divClass "core-expr-inline lambda" [] [text "\\"]
                  varEl = pre [classList [("core-expr-inline", True)]] [text var]
                  arrowEl = divClass "core-expr-inline arrow" [] [text "->"]
                  expEl = goE (lvl + 1) exp
               in divClass (cls lvl) [] [lambdaEl, varEl, arrowEl, expEl]
            Let bind exp ->
              let letEl = divClass "core-expr-inline" [] [text "let"]
                  bindEl = goB (lvl + 1) bind
                  inEl = divClass "core-expr-inline" [] [text "in"]
                  expEl = goE (lvl + 1) exp
               in divClass (cls lvl) [] [letEl, bindEl, inEl, expEl]
            Other (typ, val) ys ->
              let content = pre [] [text (T.pack (show (typ, val)))]
               in divClass (cls lvl) [] (content : fmap (goE (lvl + 1)) ys)

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
      let txt = T.pack $ drawTree $ fmap show tr
          ebind = toBind tr
          rendered =
            case ebind of
              Left err -> renderErr err
              Right bind -> renderTopBind bind
       in divClass
            "nomargin"
            []
            [ divClass "noinline" [] [pre [] [text txt]]
            , divClass "noinline" [] [rendered]
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
