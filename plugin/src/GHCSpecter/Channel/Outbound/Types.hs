{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHCSpecter.Channel.Outbound.Types
  ( -- * information types
    BreakpointLoc (..),
    TimerTag (..),
    MemInfo (..),
    Timer (..),
    getStart,
    getHscOut,
    getAs,
    getEnd,
    ModuleGraphInfo (..),
    emptyModuleGraphInfo,
    ConsoleReply (..),
    DynFlagsInfo (..),
    ProcessInfo (..),
    GhcMode (..),
    Backend (..),
    SessionInfo (..),
    emptySessionInfo,

    -- * channel
    Channel (..),
    ChanMessage (..),
    ChanMessageBox (..),
  )
where

import Data.Binary (Binary (..))
import Data.Int (Int64)
import Data.IntMap (IntMap)
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..))
import Data.Tree (Forest)
import Data.Word (Word64)
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
  | ModuleGraph
  | HsHie
  | Paused
  | Console
  deriving (Enum, Eq, Ord, Show, Generic)

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

instance Binary TimerTag where
  put tag = put (fromEnum tag)
  get = toEnum <$> get

data MemInfo = MemInfo
  { memLiveBytes :: Word64,
    memAllocCounter :: Int64
  }
  deriving (Show, Generic)

instance Binary MemInfo

newtype Timer = Timer {unTimer :: [(TimerTag, (UTCTime, Maybe MemInfo))]}
  deriving (Show, Generic, Binary)

getStart :: Timer -> Maybe (UTCTime, Maybe MemInfo)
getStart (Timer ts) = L.lookup TimerStart ts

getHscOut :: Timer -> Maybe (UTCTime, Maybe MemInfo)
getHscOut (Timer ts) = L.lookup TimerHscOut ts

getAs :: Timer -> Maybe (UTCTime, Maybe MemInfo)
getAs (Timer ts) = L.lookup TimerAs ts

getEnd :: Timer -> Maybe (UTCTime, Maybe MemInfo)
getEnd (Timer ts) = L.lookup TimerEnd ts

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
  { mginfoModuleNameMap :: IntMap ModuleName,
    mginfoModuleDep :: IntMap [Int],
    mginfoModuleTopSorted :: [Int]
  }
  deriving (Show, Read, Generic)

instance Binary ModuleGraphInfo

emptyModuleGraphInfo :: ModuleGraphInfo
emptyModuleGraphInfo = ModuleGraphInfo mempty mempty []

-- | Serializable part of DynFlags
newtype DynFlagsInfo = DynFlagsInfo
  { unDynFlagsInfo :: Text
  }
  deriving (Show, Generic)

instance Binary DynFlagsInfo

-- | GHC process info, including process id, command line arguments.
data ProcessInfo = ProcessInfo
  { procPID :: Int,
    procExecPath :: FilePath,
    procCWD :: FilePath,
    procArguments :: [String],
    procRTSFlags :: RTSFlags
  }
  deriving (Show, Generic)

instance Binary ProcessInfo

-- | This is the same as GHC.Driver.Session.GhcMode
data GhcMode = CompManager | OneShot | MkDepend
  deriving (Show, Generic)

instance Binary GhcMode

-- | This is the same as GHC.Driver.Backend.Backend
data Backend = NCG | LLVM | ViaC | Interpreter | NoBackend
  deriving (Show, Generic)

instance Binary Backend

data SessionInfo = SessionInfo
  { sessionDynFlags :: Maybe DynFlagsInfo,
    sessionProcess :: Maybe ProcessInfo,
    sessionGhcMode :: GhcMode,
    sessionBackend :: Backend,
    sessionStartTime :: Maybe UTCTime,
    sessionIsPaused :: Bool
  }
  deriving (Show, Generic)

instance Binary SessionInfo

emptySessionInfo :: SessionInfo
emptySessionInfo =
  SessionInfo
    { sessionDynFlags = Nothing,
      sessionProcess = Nothing,
      sessionGhcMode = CompManager,
      sessionBackend = NCG,
      sessionStartTime = Nothing,
      sessionIsPaused = True
    }

data ChanMessage (a :: Channel) where
  CMCheckImports :: ModuleName -> Text -> ChanMessage 'CheckImports
  CMModuleInfo :: DriverId -> ModuleName -> Maybe FilePath -> ChanMessage 'ModuleInfo
  CMTiming :: DriverId -> Timer -> ChanMessage 'Timing
  CMSession :: SessionInfo -> ChanMessage 'Session
  CMModuleGraph :: ModuleGraphInfo -> Map ModuleName FilePath -> ChanMessage 'ModuleGraph
  CMHsHie :: DriverId -> FilePath -> ChanMessage 'HsHie
  -- | a module is paused at a breakpoint position.
  CMPaused :: DriverId -> BreakpointLoc -> ChanMessage 'Paused
  CMConsole :: DriverId -> ConsoleReply -> ChanMessage 'Console

data ChanMessageBox = forall (a :: Channel). CMBox !(ChanMessage a)

instance Show ChanMessageBox where
  show (CMBox (CMCheckImports {})) = "CMCheckImports"
  show (CMBox (CMModuleInfo {})) = "CMModuleInfo"
  show (CMBox (CMTiming {})) = "CMTiming"
  show (CMBox (CMSession {})) = "CMSession"
  show (CMBox (CMModuleGraph {})) = "CMModuleGraph"
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
  put (CMBox (CMModuleGraph mgi srcs)) = do
    put (fromEnum ModuleGraph)
    put (mgi, srcs)
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
      ModuleGraph -> CMBox . uncurry CMModuleGraph <$> get
      HsHie -> CMBox . uncurry CMHsHie <$> get
      Paused -> CMBox . uncurry CMPaused <$> get
      Console -> CMBox . uncurry CMConsole <$> get
