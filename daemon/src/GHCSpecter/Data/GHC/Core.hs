{-# LANGUAGE MultiWayIf #-}

module GHCSpecter.Data.GHC.Core
  ( -- * Simplified GHC Core data type
    Id (..),
    Bind (..),
    Literal (..),
    AltCon (..),
    Alt (..),
    Expr (..),

    -- * property check
    isInlineable,
    doesNeedParens,

    -- * conversion
    toListTree,
    toVar,
    toBind,
    toLiteral,
    toAltCon,
    toAlt,
    toExpr,
  )
where

import Data.Text (Text)
import Data.Tree (Tree (..), drawTree)

-- TODO: eventually these will be isomorphic to CoreExpr

newtype Id = Id {unId :: Text}
  deriving (Show)

data Bind = Bind Id Expr
  deriving (Show)

data Literal
  = LitString Text
  | LitOther Expr
  deriving (Show)

data AltCon
  = DataAlt Text
  | LitAlt Literal
  | DEFAULT
  deriving (Show)

data Alt = Alt AltCon [Id] Expr
  deriving (Show)

data Expr
  = Var Id
  | Lit Literal
  | App Expr Expr
  | Lam Id Expr
  | Let Bind Expr
  | Case Expr Id Expr [Alt]
  | Cast Expr Expr
  | Type Expr
  | Other (Text, Text) [Expr]
  deriving (Show)

isInlineable :: Expr -> Bool
isInlineable (Var _) = True
isInlineable (Lit _) = True
isInlineable (App e1 e2) = isInlineable e1 && isInlineable e2
isInlineable (Type _) = True
isInlineable _ = False

doesNeedParens :: Expr -> Bool
doesNeedParens (Var _) = False
doesNeedParens (Lit _) = False
doesNeedParens (Type _) = False
doesNeedParens _ = True

toListTree :: Tree (Text, Text) -> Either Text [Tree (Text, Text)]
toListTree (Node (typ, val) xs)
  | typ == "Prelude.[]" =
      if
          | val == "[]" ->
              if null xs
                then pure []
                else Left "[]: not null"
          | val == "(:)" ->
              case xs of
                e1 : e2 : [] ->
                  (e1 :) <$> toListTree e2
                _ -> Left "[(:)]: not 2 children"
          | otherwise -> Left "[]: not a list constructor"
  | otherwise =
      Left "[Expr], Not a list"

-- Var is Id
toVar :: Tree (Text, Text) -> Either Text Id
toVar (Node (typ, val) xs)
  | typ == "Var" && null xs = pure (Id val)
  | otherwise = Left "Not Id"

toBind :: Tree (Text, Text) -> Either Text Bind
toBind (Node (typ, _) xs)
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
        (Node (_, sval) []) : [] -> pure (LitString sval)
        _ -> Left "LitStrign, not 1 child"
  | otherwise = LitOther <$> toExpr x

toAltCon :: Tree (Text, Text) -> Either Text AltCon
toAltCon (Node (typ, val) xs)
  | typ == "AltCon" =
      if
          | val == "DataAlt" ->
              case xs of
                Node (_, v) [] : [] -> pure $ DataAlt v
                _ -> Left "DataAlt, not a leaf node"
          | val == "LitAlt" ->
              case xs of
                l : [] -> LitAlt <$> toLiteral l
                _ -> Left "LitAlt, not a leaf node"
          | val == "DEFAULT" -> pure DEFAULT
          | otherwise -> Left "AltCon, unknown constructor"
  | otherwise = Left "not AltCon"

toAlt :: Tree (Text, Text) -> Either Text Alt
toAlt (Node (typ, _val) xs)
  | typ == "Alt" =
      case xs of
        a : is : e : [] ->
          Alt
            <$> toAltCon a
            <*> (traverse toVar =<< toListTree is)
            <*> toExpr e
        _ -> Left "Alt: not 3 children"
  | otherwise = Left "not Alt"

toExpr :: Tree (Text, Text) -> Either Text Expr
toExpr (Node (typ, val) xs)
  | typ == "Expr" =
      if
          | val == "Var" ->
              case xs of
                v : [] -> Var <$> toVar v
                _ -> Left "Var, not 1 child"
          | val == "Lit" ->
              case xs of
                l : [] -> Lit <$> toLiteral l
                _ -> Left "Lit, not 1 child"
          | val == "App" ->
              case xs of
                e1 : e2 : [] ->
                  App <$> toExpr e1 <*> toExpr e2
                _ -> Left "App, not 2 children"
          | val == "Lam" ->
              case xs of
                v : e : [] -> Lam <$> toVar v <*> toExpr e
                _ -> Left "Lam, not 2 children"
          | val == "Let" ->
              case xs of
                b : e : [] ->
                  Let <$> toBind b <*> toExpr e
                _ -> Left "Lam, not 2 children"
          | val == "Case" ->
              case xs of
                scrut : id_ : typ1 : altsExp : [] ->
                  Case
                    <$> toExpr scrut
                    <*> toVar id_
                    <*> toExpr typ1
                    <*> (traverse toAlt =<< toListTree altsExp)
                _ -> Left "Case, not 4 children"
          | val == "Cast" ->
              case xs of
                e1 : e2 : [] ->
                  Cast <$> toExpr e1 <*> toExpr e2
                _ -> Left "Cast, not 2 children"
          | val == "Type" ->
              case xs of
                t : [] -> Type <$> toExpr t
                _ -> Left "Type, not 1 child"
          | otherwise ->
              Other (typ, val) <$> traverse toExpr xs
  -- TODO: implement toType, toCoercion ..
  --  | typ == "Type" = Other (typ, val) <$> traverse toExpr xs
  | otherwise = Other (typ, val) <$> traverse toExpr xs
