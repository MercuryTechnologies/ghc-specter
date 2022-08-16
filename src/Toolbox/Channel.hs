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
    ModuleGraphInfo (..),
    emptyModuleGraphInfo,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary (..))
import Data.Binary.Instances.Time ()
import Data.IntMap (IntMap)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data Channel = TypeCheck | Timing | Session
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
  }
  deriving (Show, Generic)

instance Binary SessionInfo where
  put (SessionInfo mtime modGraph) = put (mtime, modGraph)
  get = uncurry SessionInfo <$> get

instance FromJSON SessionInfo

instance ToJSON SessionInfo

data Timer = Timer
  { timerStart :: Maybe UTCTime
  , timerEnd :: Maybe UTCTime
  }
  deriving (Show, Generic)

instance Binary Timer where
  put (Timer s t) = put (s, t)
  get = uncurry Timer <$> get

instance FromJSON Timer

instance ToJSON Timer

resetTimer :: Timer
resetTimer = Timer Nothing Nothing

data ChanMessage (a :: Channel) where
  CMTypeCheck :: ModuleName -> Text -> ChanMessage 'TypeCheck
  CMTiming :: ModuleName -> Timer -> ChanMessage 'Timing
  CMSession :: SessionInfo -> ChanMessage 'Session

data ChanMessageBox = forall (a :: Channel). CMBox !(ChanMessage a)

instance Show ChanMessageBox where
  show (CMBox (CMTypeCheck _ _)) = "CMTypeCheck"
  show (CMBox (CMTiming _ _)) = "CMTiming"
  show (CMBox (CMSession _)) = "CMSession"

instance Binary ChanMessageBox where
  put (CMBox (CMTypeCheck m t)) = do
    put (fromEnum TypeCheck)
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
      TypeCheck -> CMBox . uncurry CMTypeCheck <$> get
      Timing -> CMBox . uncurry CMTiming <$> get
      Session -> CMBox . CMSession <$> get
