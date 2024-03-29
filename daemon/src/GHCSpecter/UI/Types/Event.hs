module GHCSpecter.UI.Types.Event
  ( -- * Enums
    Tab (..),
    DetailLevel (..),
    BlockerDetailLevel (..),
    blockerThreshold,
    ComponentTag (..),
    -- ScrollDirection (..),
    SpecialKey (..),

    -- * Low-level events
    MouseEvent (..),
    KeyEvent (..),
    BackgroundEvent (..),

    -- * High-level events
    SourceViewEvent (..),
    SubModuleEvent (..),
    ModuleGraphEvent (..),
    SessionEvent (..),
    BlockerModuleGraphEvent (..),
    TimingEvent (..),
    BlockerEvent (..),
    ConsoleEvent (..),

    -- * top-level events
    UserEvent (..),
    SystemEvent (..),
    Event (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)

--
-- Enums
--

data Tab = TabSession | TabModuleGraph | TabSourceView | TabTiming | TabBlocker
  deriving (Eq, Show)

data DetailLevel = UpTo30 | UpTo100 | UpTo300
  deriving (Show, Eq, Ord, Generic)

data BlockerDetailLevel = Blocking2 | Blocking3 | Blocking4 | Blocking5
  deriving (Show, Eq, Ord, Generic)

blockerThreshold :: BlockerDetailLevel -> Int
blockerThreshold Blocking2 = 2
blockerThreshold Blocking3 = 3
blockerThreshold Blocking4 = 4
blockerThreshold Blocking5 = 5

data ComponentTag
  = TagTimingView
  | TagTimingRange
  | TagModuleGraph
  deriving (Show, Eq)

data SpecialKey
  = KeyEnter
  | KeyBackspace
  deriving (Show, Eq)

--
-- Low-level events
--

data MouseEvent
  = -- | scene_id, (x, y)
    MouseClick Text (Double, Double)
  | -- | scene_id, (x, y)
    MouseMove Text (Double, Double)
  | -- | scene_id, (x, y), (dx, dy)
    Scroll Text (Double, Double) (Double, Double)
  | -- | scene_id, (x, y), scale
    ZoomUpdate Text (Double, Double) Double
  | ZoomEnd
  deriving (Show, Eq)

data KeyEvent
  = NormalKeyPressed Text
  | SpecialKeyPressed SpecialKey
  deriving (Show, Eq)

data BackgroundEvent
  = MessageChanUpdated
  | RefreshUI
  deriving (Show, Eq)

--
-- High-level events
--

data SourceViewEvent
  = SelectModule ModuleName
  | UnselectModule
  | SetBreakpoint ModuleName Bool
  | SourceViewTab (Text, Int)
  deriving (Show, Eq)

data ModuleGraphEvent
  = HoverOnModuleEv (Maybe ModuleName)
  | ClickOnModuleEv (Maybe ModuleName)
  deriving (Show, Eq)

data SubModuleEvent
  = SubModuleGraphEv ModuleGraphEvent
  | SubModuleLevelEv DetailLevel
  deriving (Show, Eq)

data SessionEvent
  = SaveSessionEv
  | ResumeSessionEv
  | PauseSessionEv
  deriving (Show, Eq)

data BlockerModuleGraphEvent
  = BMGGraph ModuleGraphEvent
  | BMGUpdateLevel BlockerDetailLevel
  deriving (Show, Eq)

data TimingEvent
  = ToCurrentTime
  | -- | is thawed (i.e. flowing) = True, is frozen = False
    TimingFlow Bool
  | UpdatePartition Bool
  | UpdateParallel Bool
  | HoverOnModule ModuleName
  | HoverOffModule ModuleName
  deriving (Show, Eq)

data BlockerEvent
  = ComputeBlockerGraph
  | BlockerModuleGraphEv BlockerModuleGraphEvent
  deriving (Show, Eq)

data ConsoleEvent k
  = ConsoleTab k
  | ConsoleKey Text
  | ConsoleInput Text
  | -- | True: send the command immediately, False: wait for user's Enter.
    ConsoleButtonPressed Bool Text
  | -- TODO: we should have these cases into one and make each case as a nested type.
    ConsoleDumpTiming
  | ConsoleDumpMemory
  | ConsoleDumpModGraph
  deriving (Show, Eq)

data UserEvent
  = TabEv Tab
  | SourceViewEv SourceViewEvent
  | MainModuleEv ModuleGraphEvent
  | SubModuleEv SubModuleEvent
  | SessionEv SessionEvent
  | TimingEv TimingEvent
  | BlockerEv BlockerEvent
  | MouseEv MouseEvent
  | KeyEv KeyEvent
  | ConsoleEv (ConsoleEvent DriverId)
  | DummyEv
  deriving (Show, Eq)

data SystemEvent = BkgEv BackgroundEvent
  deriving (Show, Eq)

data Event = UsrEv UserEvent | SysEv SystemEvent
  deriving (Show, Eq)
