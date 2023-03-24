{-# LANGUAGE MultiWayIf #-}

module GHCSpecter.Data.GHC.Core (
  -- * Simplified GHC Core data type
  Id (..),
  Bind (..),
  Literal (..),
  AltCon (..),
  Alt (..),
  Expr (..),

  -- * conversion
  toListTree,
  toVar,
  toBind,
  toLiteral,
  toAltCon,
  toAlt,
  toExpr,
) where

import Control.Monad ((<=<))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..))
import GHCSpecter.Util.GHC (
  coreTypeAlt,
  coreTypeAltCon,
  coreTypeBind,
  coreTypeExpr,
  coreTypeLiteral,
 )
import Text.Read (readMaybe)

-- TODO: eventually these will be isomorphic to CoreExpr

newtype Id = Id {unId :: Text}
  deriving (Show)

data Bind
  = NonRec Id Expr
  | Rec [(Id, Expr)]
  deriving (Show)

data Literal
  = LitNumber Text Integer
  | LitString Text
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
toBind (Node (typ, val) xs)
  | typ == coreTypeBind =
      if
          | val == "NonRec" ->
              case xs of
                v : e : [] -> NonRec <$> toVar v <*> toExpr e
                _ -> Left "Bind:NonRec, not 2 children"
          | val == "Rec" ->
              case xs of
                bs : [] -> do
                  trs <- toListTree bs
                  Rec <$> traverse (toBTuple <=< to2Tuple) trs
                _ -> Left "Bind:Rec, not 1 child"
          | otherwise -> Left "Bind, unknown constructor"
  | otherwise = Left "not Bind"
  where
    to2Tuple (Node (typ', _) xs')
      | typ' == "(,)" =
          case xs' of
            e1 : e2 : [] -> pure (e1, e2)
            _ -> Left "Bind:Rec: 2-tuple, not 2 children"
      | otherwise = Left "Bind:Rec: not 2-tuple"
    toBTuple (v, e) =
      (,) <$> toVar v <*> toExpr e

toLiteral :: Tree (Text, Text) -> Either Text Literal
toLiteral x@(Node (typ, val) xs)
  | typ == coreTypeLiteral =
      if
          | val == "LitString" ->
              case xs of
                (Node (_, sval) []) : [] -> pure (LitString sval)
                _ -> Left "LitString, not 1 child"
          | val == "LitNumber" ->
              case xs of
                Node (_, ntyp) [] : Node (_, nnum) [] : [] ->
                  let mnum = readMaybe (T.unpack nnum)
                   in case mnum of
                        Nothing -> Left "LitNumber, not integer"
                        Just num -> pure $ LitNumber ntyp num
                _ -> Left "LitNumber, not 2 children"
          | otherwise ->
              LitOther <$> toExpr x
  | otherwise = Left "Literal: not Literal"

toAltCon :: Tree (Text, Text) -> Either Text AltCon
toAltCon (Node (typ, val) xs)
  | typ == coreTypeAltCon =
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
  | typ == coreTypeAlt =
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
  | typ == coreTypeExpr =
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
