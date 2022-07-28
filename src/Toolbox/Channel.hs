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
import Data.Binary.Instances.Time ()
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

data Channel = CheckImports | Timing
  deriving (Enum, Eq, Ord, Show)

type ModuleName = Text

data ChanMessage (a :: Channel) where
  CMCheckImports :: ModuleName -> Text -> ChanMessage 'CheckImports
  CMTiming :: ModuleName -> UTCTime -> ChanMessage 'Timing

data ChanMessageBox = forall (a :: Channel). CMBox !(ChanMessage a)

instance Show ChanMessageBox where
  show (CMBox (CMCheckImports _ _)) = "CMCheckImports"
  show (CMBox (CMTiming _ _)) = "CMTiming"

instance Binary ChanMessageBox where
  put (CMBox (CMCheckImports m t)) = do
    put (fromEnum CheckImports)
    put (m, t)
  put (CMBox (CMTiming m t)) = do
    put (fromEnum Timing)
    put (m, t)

  get = do
    tag <- get
    case toEnum tag of
      CheckImports -> CMBox . uncurry CMCheckImports <$> get
      Timing -> CMBox . uncurry CMTiming <$> get
