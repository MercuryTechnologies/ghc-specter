{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module GHCSpecter.Channel.Outbound.Types
  ( -- * information types
    BreakpointLoc (..),
    SessionInfo (..),
    TimerTag (..),
    Timer (..),
    getStartTime,
    getHscOutTime,
    getAsTime,
    getEndTime,
    HsSourceInfo (..),
    ModuleGraphInfo (..),
    emptyModuleGraphInfo,
    ConsoleReply (..),

    -- * channel
    Channel (..),
    ChanMessage (..),
    ChanMessageBox (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary (..))
import Data.Binary.Instances.Time ()
import Data.IntMap (IntMap)
import Data.List qualified as L
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Tree (Forest)
import GHC.Generics (Generic)
import GHCSpecter.Channel.Common.Types
  ( DriverId (..),
    type ModuleName,
  )

data Channel
  = CheckImports
  | ModuleInfo
  | Timing
  | Session
  | HsSource
  | Paused
  | Console
  deriving (Enum, Eq, Ord, Show, Generic)

instance FromJSON Channel

instance ToJSON Channel

data BreakpointLoc
  = StartDriver
  | AfterParser
  | Typecheck
  | Core2Core Text
  | PreRunPhase Text
  | PostRunPhase Text
  deriving (Show, Eq, Generic)

instance Binary BreakpointLoc

instance FromJSON BreakpointLoc

instance ToJSON BreakpointLoc

data ModuleGraphInfo = ModuleGraphInfo
  { mginfoModuleNameMap :: IntMap ModuleName
  , mginfoModuleDep :: IntMap [Int]
  , mginfoModuleTopSorted :: [Int]
  }
  deriving (Show, Read, Generic)

instance FromJSON ModuleGraphInfo

instance ToJSON ModuleGraphInfo

instance Binary ModuleGraphInfo where
  put (ModuleGraphInfo m d s) = put (m, d, s)
  get = (\(m, d, s) -> ModuleGraphInfo m d s) <$> get

emptyModuleGraphInfo :: ModuleGraphInfo
emptyModuleGraphInfo = ModuleGraphInfo mempty mempty []

data SessionInfo = SessionInfo
  { sessionProcessId :: Int
  , sessionStartTime :: Maybe UTCTime
  , sessionModuleGraph :: ModuleGraphInfo
  , sessionIsPaused :: Bool
  }
  deriving (Show, Generic)

instance Binary SessionInfo where
  put (SessionInfo pid mtime modGraph isPaused) = put (pid, mtime, modGraph, isPaused)
  get = (\(pid, mtime, modGraph, isPaused) -> SessionInfo pid mtime modGraph isPaused) <$> get

instance FromJSON SessionInfo

instance ToJSON SessionInfo

data TimerTag
  = -- | start
    TimerStart
  | -- | Haskell compiler finished
    TimerHscOut
  | -- | Assembler phase
    TimerAs
  | -- | StopLn phase
    TimerEnd
  deriving (Enum, Eq, Ord, Generic, Show)

instance FromJSON TimerTag

instance ToJSON TimerTag

instance Binary TimerTag where
  put tag = put (fromEnum tag)
  get = toEnum <$> get

newtype Timer = Timer {unTimer :: [(TimerTag, UTCTime)]}
  deriving (Show, Generic, Binary, FromJSON, ToJSON)

getStartTime :: Timer -> Maybe UTCTime
getStartTime (Timer ts) = L.lookup TimerStart ts

getHscOutTime :: Timer -> Maybe UTCTime
getHscOutTime (Timer ts) = L.lookup TimerHscOut ts

getAsTime :: Timer -> Maybe UTCTime
getAsTime (Timer ts) = L.lookup TimerAs ts

getEndTime :: Timer -> Maybe UTCTime
getEndTime (Timer ts) = L.lookup TimerEnd ts

newtype HsSourceInfo = HsSourceInfo
  { hsHieFile :: FilePath
  }
  deriving (Show, Generic)

instance Binary HsSourceInfo where
  put (HsSourceInfo hie) = put hie
  get = HsSourceInfo <$> get

instance FromJSON HsSourceInfo

instance ToJSON HsSourceInfo

data ConsoleReply
  = ConsoleReplyText Text
  | ConsoleReplyButton [[(Text, Text)]]
  | ConsoleReplyCore (Forest (Text, Text))
  deriving (Show, Generic)

instance Binary ConsoleReply

data ChanMessage (a :: Channel) where
  CMCheckImports :: ModuleName -> Text -> ChanMessage 'CheckImports
  CMModuleInfo :: DriverId -> ModuleName -> ChanMessage 'ModuleInfo
  CMTiming :: DriverId -> Timer -> ChanMessage 'Timing
  CMSession :: SessionInfo -> ChanMessage 'Session
  CMHsSource :: DriverId -> HsSourceInfo -> ChanMessage 'HsSource
  CMPaused :: DriverId -> Maybe BreakpointLoc -> ChanMessage 'Paused
  CMConsole :: DriverId -> ConsoleReply -> ChanMessage 'Console

data ChanMessageBox = forall (a :: Channel). CMBox !(ChanMessage a)

instance Show ChanMessageBox where
  show (CMBox (CMCheckImports {})) = "CMCheckImports"
  show (CMBox (CMModuleInfo {})) = "CMModuleInfo"
  show (CMBox (CMTiming {})) = "CMTiming"
  show (CMBox (CMSession {})) = "CMSession"
  show (CMBox (CMHsSource {})) = "CMHsSource"
  show (CMBox (CMPaused {})) = "CMPaused"
  show (CMBox (CMConsole {})) = "CMConsole"

instance Binary ChanMessageBox where
  put (CMBox (CMCheckImports m t)) = do
    put (fromEnum CheckImports)
    put (m, t)
  put (CMBox (CMModuleInfo i m)) = do
    put (fromEnum ModuleInfo)
    put (i, m)
  put (CMBox (CMTiming i t)) = do
    put (fromEnum Timing)
    put (i, t)
  put (CMBox (CMSession s)) = do
    put (fromEnum Session)
    put s
  put (CMBox (CMHsSource i h)) = do
    put (fromEnum HsSource)
    put (i, h)
  put (CMBox (CMPaused i ml)) = do
    put (fromEnum Paused)
    put (i, ml)
  put (CMBox (CMConsole i t)) = do
    put (fromEnum Console)
    put (i, t)

  get = do
    tag <- get
    case toEnum tag of
      CheckImports -> CMBox . uncurry CMCheckImports <$> get
      ModuleInfo -> CMBox . uncurry CMModuleInfo <$> get
      Timing -> CMBox . uncurry CMTiming <$> get
      Session -> CMBox . CMSession <$> get
      HsSource -> CMBox . uncurry CMHsSource <$> get
      Paused -> CMBox . uncurry CMPaused <$> get
      Console -> CMBox . uncurry CMConsole <$> get
