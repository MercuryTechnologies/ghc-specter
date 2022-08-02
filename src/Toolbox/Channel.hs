{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Toolbox.Channel
  ( Channel (..),
    type ModuleName,
    ChanMessage (..),
    ChanMessageBox (..),
  )
where

import Data.Binary (Binary (..))
import Data.Text (Text)

data Channel = CheckImports | Trivial
  deriving (Enum, Eq, Ord, Show)

type ModuleName = Text

data ChanMessage (a :: Channel) where
  CMCheckImports :: ModuleName -> Text -> ChanMessage 'CheckImports
  CMTrivial :: ModuleName -> ChanMessage 'Trivial

data ChanMessageBox = forall (a :: Channel). CMBox !(ChanMessage a)

instance Show ChanMessageBox where
  show (CMBox (CMCheckImports _ _)) = "CMCheckImports"
  show (CMBox (CMTrivial _)) = "CMTrivial"

instance Binary ChanMessageBox where
  put (CMBox (CMCheckImports m t)) = do
    put (fromEnum CheckImports)
    put (m, t)
  put (CMBox (CMTrivial m)) = do
    put (fromEnum Trivial)
    put m

  get = do
    tag <- get
    case toEnum tag of
      CheckImports -> CMBox . uncurry CMCheckImports <$> get
      Trivial -> CMBox . CMTrivial <$> get
