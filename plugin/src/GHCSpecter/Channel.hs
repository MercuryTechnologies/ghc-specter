{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module GHCSpecter.Channel
  ( -- * information types
    type ModuleName,
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
import GHC.Generics (Generic)

data Channel = CheckImports | Timing | Session | HsSource
  deriving (Enum, Eq, Ord, Show, Generic)

instance FromJSON Channel

instance ToJSON Channel

type ModuleName = Text

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
  { sessionStartTime :: Maybe UTCTime
  , sessionModuleGraph :: ModuleGraphInfo
  , sessionIsPaused :: Bool
  }
  deriving (Show, Generic)

instance Binary SessionInfo where
  put (SessionInfo mtime modGraph isPaused) = put (mtime, modGraph, isPaused)
  get = (\(mtime, modGraph, isPaused) -> SessionInfo mtime modGraph isPaused) <$> get

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

data ChanMessage (a :: Channel) where
  CMCheckImports :: ModuleName -> Text -> ChanMessage 'CheckImports
  CMTiming :: ModuleName -> Timer -> ChanMessage 'Timing
  CMSession :: SessionInfo -> ChanMessage 'Session
  CMHsSource :: ModuleName -> HsSourceInfo -> ChanMessage 'HsSource

data ChanMessageBox = forall (a :: Channel). CMBox !(ChanMessage a)

instance Show ChanMessageBox where
  show (CMBox (CMCheckImports {})) = "CMCheckImports"
  show (CMBox (CMTiming {})) = "CMTiming"
  show (CMBox (CMSession {})) = "CMSession"
  show (CMBox (CMHsSource {})) = "CMHsSource"

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
  put (CMBox (CMHsSource m h)) = do
    put (fromEnum HsSource)
    put (m, h)

  get = do
    tag <- get
    case toEnum tag of
      CheckImports -> CMBox . uncurry CMCheckImports <$> get
      Timing -> CMBox . uncurry CMTiming <$> get
      Session -> CMBox . CMSession <$> get
      HsSource -> CMBox . uncurry CMHsSource <$> get
