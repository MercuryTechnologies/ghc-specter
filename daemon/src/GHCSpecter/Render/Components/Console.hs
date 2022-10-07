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

-- TODO: eventually these will be isomorphic to CoreExpr

newtype Id = Id {unId :: Text}
  deriving (Show)

data Bind = Bind Id Expr
  deriving (Show)

data Literal
  = LitString Text
  | LitOther Expr
  deriving (Show)

data Expr
  = Var Id
  | Lit Literal
  | App Expr Expr
  | Lam Id Expr
  | Let Bind Expr
  | Other (Text, Text) [Expr]
  deriving (Show)

isInlineable :: Expr -> Bool
isInlineable (Var _) = True
isInlineable (Lit _) = True
isInlineable _ = False

-- Var is Id
toVar :: Tree (Text, Text) -> Either Text Id
toVar (Node (typ, val) xs)
  | typ == "Var" && null xs = pure (Id val)
  | otherwise = Left "Not Id"

toBind :: Tree (Text, Text) -> Either Text Bind
toBind (Node (typ, val) xs)
  | typ == "Bind" =
      case xs of
        v : e : [] -> Bind <$> toVar v <*> toExpr e
        -- TODO: should handle recursive case
        _ -> Left "Bind, not 2 children"
  | otherwise = Left "not Bind"

toLiteral :: Tree (Text, Text) -> Either Text Literal
toLiteral x@(Node (typ, val) xs)
  | typ == "Literal" && val == "LitString" =
      case xs of
        (Node (styp, sval) []) : [] -> pure (LitString sval)
        _ -> Left "LitStrign, not 1 child"
  | otherwise = LitOther <$> toExpr x

toExpr :: Tree (Text, Text) -> Either Text Expr
toExpr x@(Node (typ, val) xs)
  | typ == "Expr" =
      if
          | val == "Var" -> do
              case xs of
                v : [] -> Var <$> toVar v
                _ -> Left "Var, not 1 child"
          | val == "Lit" -> do
              case xs of
                l : [] -> Lit <$> toLiteral l
                _ -> Left "Lit, not 1 child"
          | val == "App" -> do
              case xs of
                e1 : e2 : [] ->
                  App <$> toExpr e1 <*> toExpr e2
                _ -> Left "App, not 2 children"
          | val == "Lam" -> do
              case xs of
                v : e : [] -> Lam <$> toVar v <*> toExpr e
                _ -> Left "Lam, not 2 children"
          | val == "Let" -> do
              case xs of
                b : e : [] ->
                  Let <$> toBind b <*> toExpr e
                _ -> Left "Lam, not 2 children"
          | otherwise ->
              Other (typ, val) <$> traverse toExpr xs
  -- TODO: implement toType, toCoercion ..
  --  | typ == "Type" = Other (typ, val) <$> traverse toExpr xs
  | otherwise = Other (typ, val) <$> traverse toExpr xs -- Left "Not Expr"

renderTopBind :: Bind -> Widget IHTML a
renderTopBind bind = goB 0 bind
  where
    cls l = if l == 0 then "core-expr top" else "core-expr"

    goB lvl (Bind var exp) =
      let varEl = pre [classList [("core-expr-inline", True)]] [text (unId var)]
          eqEl = divClass "core-expr-inline eq" [] [text "="]
          expEl = goE (lvl + 1) exp
       in divClass (cls lvl) [] [varEl, eqEl, expEl]

    space = divClass "core-expr-inline space" [] []

    renderApp lvl e1 e2
      | isInlineable e1 && isInlineable e2 =
          let e1El = divClass "core-expr-inline" [] [goE lvl e1]
              space = divClass "core-expr-inline space" [] []
              e2El = divClass "core-expr-inline" [] [goE lvl e2]
           in divClass (cls lvl) [] [e1El, space, e2El]
      | isInlineable e1 =
          let e1El = divClass "core-expr-inline" [] [goE lvl e1]
              parenLEl = divClass "core-expr-inline paren" [] [text "("]
              parenREl = divClass "core-expr-inline paren" [] [text ")"]
              e2El = goE (lvl + 1) e2
           in divClass
                (cls lvl)
                []
                [e1El, space, parenLEl, space, e2El, space, parenREl]
      | otherwise =
          let appEl = divClass "core-expr-inline" [] [text "App"]
              e1El = goE (lvl + 1) e1
              e2El = goE (lvl + 1) e2
           in divClass (cls lvl) [] [appEl, e1El, e2El]

    goE lvl expr =
      case expr of
        Var var ->
          pre [style [("margin", "0"), ("padding", "0")]] [text (unId var)]
        -- special treatment for readability
        Lit (LitString txt) ->
          let txt' = "\"" <> txt <> "\""
           in pre [style [("margin", "0"), ("padding", "0")]] [text txt']
        Lit (LitOther e) ->
          goE lvl e
        App e1 e2 -> renderApp lvl e1 e2
        Lam var exp ->
          let lambdaEl = divClass "core-expr-inline lambda" [] [text "\\"]
              varEl = pre [classList [("core-expr-inline", True)]] [text (unId var)]
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
