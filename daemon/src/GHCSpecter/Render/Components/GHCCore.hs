module GHCSpecter.Render.Components.GHCCore
  ( renderTopBind,
  )
where

import Concur.Core (Widget)
import Data.List qualified as L
import Data.Text qualified as T
import GHCSpecter.Data.GHC.Core
  ( Alt (..),
    AltCon (..),
    Bind (..),
    Expr (..),
    Id (..),
    Literal (..),
    doesNeedParens,
    isInlineable,
  )
import GHCSpecter.Render.Util (divClass, spanClass)
import GHCSpecter.UI.ConcurReplica.DOM
  ( pre,
    span,
    text,
  )
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import Prelude hiding (div, span)

renderTopBind :: Bind -> Widget IHTML a
renderTopBind bind = goB 0 bind
  where
    cls l = if l == 0 then "core-expr top" else "core-expr"
    space = spanClass "space" [] [text " "]
    eqEl = spanClass "eq" [] [text "="]
    parenLEl = spanClass "paren" [] [text "("]
    parenREl = spanClass "paren" [] [text ")"]

    wrapParen (e, rendered)
      | doesNeedParens e = [parenLEl, rendered, parenREl]
      | otherwise = [rendered]

    goB :: Int -> Bind -> Widget IHTML a
    goB lvl (Bind var expr) =
      let varEl = span [] [text (unId var)]
          expEl = goE (lvl + 1) expr
       in divClass (cls lvl) [] [varEl, eqEl, expEl]

    goApp lvl e1 e2
      | isInlineable e1 && isInlineable e2 =
          let e1El = span [] [goE lvl e1]
              e2El = span [] [goE lvl e2]
           in span [] (wrapParen (e1, e1El) ++ [space] ++ wrapParen (e2, e2El))
      | isInlineable e1 =
          let e1El = span [] [goE lvl e1]
              e2El = goE (lvl + 1) e2
           in divClass
                (cls lvl)
                []
                (wrapParen (e1, e1El) ++ [space, parenLEl, e2El, parenREl])
      | otherwise =
          let e1El = goE (lvl + 1) e1
              e2El = goE (lvl + 1) e2
           in divClass
                (cls lvl)
                []
                [ parenLEl
                , e1El
                , parenREl
                , space
                , parenLEl
                , e2El
                , parenREl
                ]

    goAlt lvl (Alt con ids expr) =
      let conTxt =
            case con of
              DataAlt txt -> txt
              LitAlt (LitString txt) -> txt
              LitAlt _ -> "LitOther"
              DEFAULT -> "DEFAULT"
          conEl = span [] [text conTxt]
          idsEl = fmap (\i -> span [] [text (unId i)]) ids
          arrowEl = spanClass "arrow" [] [text "->"]
          expEl = goE (lvl + 1) expr
       in divClass
            (cls lvl)
            []
            (L.intersperse space ((conEl : idsEl) ++ [arrowEl, expEl]))

    goCase lvl scrut _i _t alts =
      let caseEl = span [] [text "case"]
          ofEl = span [] [text "of"]
          scrutEl = goE (lvl + 1) scrut
          altEls = fmap (goAlt (lvl + 1)) alts
       in divClass (cls lvl) [] ([caseEl, space, scrutEl, space, ofEl] ++ altEls)

    goE lvl expr =
      case expr of
        Var var -> span [] [text (unId var)]
        -- special treatment for readability
        Lit (LitString txt) ->
          let txt' = "\"" <> txt <> "\""
           in span [] [text txt']
        Lit (LitOther e) ->
          goE lvl e
        App e1 e2 -> goApp lvl e1 e2
        Lam var expr' ->
          let lambdaEl = spanClass "lambda" [] [text "\\"]
              varEl = span [] [text (unId var)]
              arrowEl = spanClass "arrow" [] [text "->"]
              expEl = goE (lvl + 1) expr'
           in divClass (cls lvl) [] [lambdaEl, varEl, arrowEl, expEl]
        Let bind' expr' ->
          let letEl = span [] [text "let"]
              bindEl = goB (lvl + 1) bind'
              inEl = span [] [text "in"]
              expEl = goE (lvl + 1) expr'
           in divClass (cls lvl) [] [letEl, bindEl, inEl, expEl]
        Case scrut id_ typ alts -> goCase lvl scrut id_ typ alts
        -- ignore Coercion for now
        -- TODO: will be available as user asks.
        Cast e _ -> goE lvl e
        -- ignore Type for now
        -- TODO: will be available as user asks.
        Type _ -> span [] [text "Type"]
        Other (typ, val) ys ->
          let content = pre [] [text (T.pack (show (typ, val)))]
           in divClass (cls lvl) [] (content : fmap (goE (lvl + 1)) ys)
