{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Toolbox.Channel
  ( Channel (..),
    type ModuleName,
    ChanMessage (..),
    ChanMessageBox (..),
    SessionInfo (..),
    Timer (..),
    resetTimer,
  )
where

import Data.Binary (Binary (..))
import Data.Binary.Instances.Time ()
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

data Channel = CheckImports | Timing | Session
  deriving (Enum, Eq, Ord, Show)

type ModuleName = Text

newtype SessionInfo = SessionInfo
  {sessionStartTime :: Maybe UTCTime}
  deriving (Show, Binary)

data Timer = Timer
  { timerStart :: Maybe UTCTime
  , timerEnd :: Maybe UTCTime
  }
  deriving (Show)

instance Binary Timer where
  put (Timer s t) = put (s, t)
  get = uncurry Timer <$> get

resetTimer :: Timer
resetTimer = Timer Nothing Nothing

data ChanMessage (a :: Channel) where
  CMCheckImports :: ModuleName -> Text -> ChanMessage 'CheckImports
  CMTiming :: ModuleName -> Timer -> ChanMessage 'Timing
  CMSession :: SessionInfo -> ChanMessage 'Session

data ChanMessageBox = forall (a :: Channel). CMBox !(ChanMessage a)

instance Show ChanMessageBox where
  show (CMBox (CMCheckImports _ _)) = "CMCheckImports"
  show (CMBox (CMTiming _ _)) = "CMTiming"
  show (CMBox (CMSession _)) = "CMSession"

instance Binary ChanMessageBox where
  put (CMBox (CMCheckImports m t)) = do
    put (fromEnum CheckImports)
    put (m, t)
  put (CMBox (CMTiming m t)) = do
    put (fromEnum Timing)
    put (m, t)
  put (CMBox (CMSession s)) = do
    put (fromEnum Session)
    put s

  get = do
    tag <- get
    case toEnum tag of
      CheckImports -> CMBox . uncurry CMCheckImports <$> get
      Timing -> CMBox . uncurry CMTiming <$> get
      Session -> CMBox . CMSession <$> get
