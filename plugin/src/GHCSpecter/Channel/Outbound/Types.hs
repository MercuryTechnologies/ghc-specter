{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import Data.Aeson qualified as A
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
import GHC.RTS.Flags
  ( CCFlags,
    ConcFlags,
    DebugFlags,
    DoCostCentres,
    DoHeapProfile,
    DoTrace,
    GCFlags,
    GiveGCStats,
    IoSubSystem (..),
    MiscFlags,
    ParFlags,
    ProfFlags,
    RTSFlags,
    TickyFlags,
    TraceFlags,
  )
import GHCSpecter.Channel.Common.Types
  ( DriverId (..),
    type ModuleName,
  )

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

-- orphan instances
instance Binary GiveGCStats

instance FromJSON GiveGCStats

instance ToJSON GiveGCStats

instance Binary GCFlags

instance FromJSON GCFlags

instance ToJSON GCFlags

instance Binary CCFlags

instance FromJSON CCFlags

instance ToJSON CCFlags

instance Binary ConcFlags

instance FromJSON ConcFlags

instance ToJSON ConcFlags

instance Binary DebugFlags

instance FromJSON DebugFlags

instance ToJSON DebugFlags

instance Binary DoCostCentres

instance FromJSON DoCostCentres

instance ToJSON DoCostCentres

instance Binary DoHeapProfile

instance FromJSON DoHeapProfile

instance ToJSON DoHeapProfile

instance Binary DoTrace

instance FromJSON DoTrace

instance ToJSON DoTrace

-- instance Generic IoSubSystem

instance Binary IoSubSystem where
  put = put . fromEnum
  get = toEnum <$> get

instance FromJSON IoSubSystem where
  parseJSON =
    A.withText "IoSubSystem" $ \txt ->
      if | txt == "IoPOSIX" -> pure IoPOSIX
         | txt == "IoNative" -> pure IoNative
         | otherwise -> fail "cannot parse IoSubSystem"

instance ToJSON IoSubSystem where
  toJSON IoPOSIX = A.String "IoPOSIX"
  toJSON IoNative = A.String "IoNative"

instance Binary MiscFlags

instance FromJSON MiscFlags

instance ToJSON MiscFlags

instance Binary ParFlags

instance FromJSON ParFlags

instance ToJSON ParFlags

instance Binary ProfFlags

instance FromJSON ProfFlags

instance ToJSON ProfFlags

instance Binary TickyFlags

instance FromJSON TickyFlags

instance ToJSON TickyFlags

instance Binary TraceFlags

instance FromJSON TraceFlags

instance ToJSON TraceFlags

instance Binary RTSFlags

instance FromJSON RTSFlags

instance ToJSON RTSFlags

-- | GHC process info, including process id, command line arguments.
data ProcessInfo = ProcessInfo
  { procPID :: Int
  , procExecPath  :: FilePath
  , procCWD :: FilePath
  , procArguments :: [String]
  -- , procRTSFlags :: RTSFlags
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
