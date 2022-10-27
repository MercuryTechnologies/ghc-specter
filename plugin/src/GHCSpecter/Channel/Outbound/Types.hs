{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module GHCSpecter.Channel.Outbound.Types
  ( -- * information types
    BreakpointLoc (..),
    TimerTag (..),
    Timer (..),
    getStartTime,
    getHscOutTime,
    getAsTime,
    getEndTime,
    ModuleGraphInfo (..),
    emptyModuleGraphInfo,
    ConsoleReply (..),
    ProcessInfo (..),
    SessionInfo (..),
    emptySessionInfo,

    -- * channel
    Channel (..),
    ChanMessage (..),
    ChanMessageBox (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Binary (Binary (..))
import Data.Binary.Instances.Time ()
import Data.IntMap (IntMap)
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Tree (Forest)
import GHC.Generics (Generic)
import GHC.RTS.Flags (RTSFlags)
import GHCSpecter.Channel.Common.Types
  ( DriverId (..),
    type ModuleName,
  )
import GHCSpecter.Data.GHC.Orphans ()

data Channel
  = CheckImports
  | ModuleInfo
  | Timing
  | Session
  | HsHie
  | Paused
  | Console
  deriving (Enum, Eq, Ord, Show, Generic)

instance FromJSON Channel

instance ToJSON Channel

data BreakpointLoc
  = StartDriver
  | ParsedResultAction
  | RenamedResultAction
  | PreRunMeta
  | SpliceRunAction
  | PostRunMeta
  | RnSplice
  | TypecheckInit
  | TypecheckSolve
  | TypecheckStop
  | TypecheckResultAction
  | Core2Core Text
  | PreRunPhase Text
  | -- | (before_phase, after_phase)
    PostRunPhase (Text, Text)
  deriving (Show, Eq, Generic)

instance Binary BreakpointLoc

instance FromJSON BreakpointLoc

instance ToJSON BreakpointLoc

-- TODO: GHC 9.4 changed compilation phase semantics considerably.
-- We need to introduce GHC version dependent tag concept.
-- This timer tag is incomplete after all.
data TimerTag
  = -- | start
    TimerStart
  | -- | Haskell compiler finished
    TimerHscOut
  | -- | Assembler phase start
    TimerAs
  | -- | StopLn phase i.e. compilation end
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

data ConsoleReply
  = -- | simple textual reply (with name optionally)
    ConsoleReplyText (Maybe Text) Text
  | -- | list of Core bind items. The items in the same inner list are mutually
    -- recursive binding, i.e. should be presented together.
    ConsoleReplyCoreBindList [[Text]]
  | -- | core tree
    ConsoleReplyCore (Forest (Text, Text))
  deriving (Show, Generic)

instance Binary ConsoleReply

data ModuleGraphInfo = ModuleGraphInfo
  { mginfoModuleNameMap :: IntMap ModuleName
  , mginfoModuleDep :: IntMap [Int]
  , mginfoModuleTopSorted :: [Int]
  }
  deriving (Show, Read, Generic)

instance FromJSON ModuleGraphInfo

instance ToJSON ModuleGraphInfo

instance Binary ModuleGraphInfo

emptyModuleGraphInfo :: ModuleGraphInfo
emptyModuleGraphInfo = ModuleGraphInfo mempty mempty []

-- | GHC process info, including process id, command line arguments.
data ProcessInfo = ProcessInfo
  { procPID :: Int
  , procExecPath  :: FilePath
  , procCWD :: FilePath
  , procArguments :: [String]
  , procRTSFlags :: RTSFlags
  }
  deriving (Show, Generic)

instance Binary ProcessInfo

instance FromJSON ProcessInfo

instance ToJSON ProcessInfo

data SessionInfo = SessionInfo
  { sessionProcess :: Maybe ProcessInfo
  , sessionStartTime :: Maybe UTCTime
  , sessionModuleGraph :: ModuleGraphInfo
  , sessionModuleSources :: Map ModuleName FilePath
  , sessionIsPaused :: Bool
  }
  deriving (Show, Generic)

instance Binary SessionInfo

instance FromJSON SessionInfo

instance ToJSON SessionInfo

emptySessionInfo :: SessionInfo
emptySessionInfo =
  SessionInfo Nothing Nothing emptyModuleGraphInfo M.empty True

data ChanMessage (a :: Channel) where
  CMCheckImports :: ModuleName -> Text -> ChanMessage 'CheckImports
  CMModuleInfo :: DriverId -> ModuleName -> Maybe FilePath -> ChanMessage 'ModuleInfo
  CMTiming :: DriverId -> Timer -> ChanMessage 'Timing
  CMSession :: SessionInfo -> ChanMessage 'Session
  CMHsHie :: DriverId -> FilePath -> ChanMessage 'HsHie
  CMPaused :: DriverId -> Maybe BreakpointLoc -> ChanMessage 'Paused
  CMConsole :: DriverId -> ConsoleReply -> ChanMessage 'Console

data ChanMessageBox = forall (a :: Channel). CMBox !(ChanMessage a)

instance Show ChanMessageBox where
  show (CMBox (CMCheckImports {})) = "CMCheckImports"
  show (CMBox (CMModuleInfo {})) = "CMModuleInfo"
  show (CMBox (CMTiming {})) = "CMTiming"
  show (CMBox (CMSession {})) = "CMSession"
  show (CMBox (CMHsHie {})) = "CMHsHie"
  show (CMBox (CMPaused {})) = "CMPaused"
  show (CMBox (CMConsole {})) = "CMConsole"

instance Binary ChanMessageBox where
  put (CMBox (CMCheckImports m t)) = do
    put (fromEnum CheckImports)
    put (m, t)
  put (CMBox (CMModuleInfo i m mf)) = do
    put (fromEnum ModuleInfo)
    put (i, m, mf)
  put (CMBox (CMTiming i t)) = do
    put (fromEnum Timing)
    put (i, t)
  put (CMBox (CMSession s)) = do
    put (fromEnum Session)
    put s
  put (CMBox (CMHsHie i h)) = do
    put (fromEnum HsHie)
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
      ModuleInfo -> CMBox . (\(i, m, mf) -> CMModuleInfo i m mf) <$> get
      Timing -> CMBox . uncurry CMTiming <$> get
      Session -> CMBox . CMSession <$> get
      HsHie -> CMBox . uncurry CMHsHie <$> get
      Paused -> CMBox . uncurry CMPaused <$> get
      Console -> CMBox . uncurry CMConsole <$> get
