module GHCSpecter.Control.Types
  ( -- * eDSL Types
    ControlF (..),
    Control,

    -- * Primitive operations of eDSL
    getState,
    putState,
    nextEvent,
    printMsg,
    getCurrentTime,
    getLastUpdatedUI,
    shouldUpdate,
    saveSession,
  )
where

import Control.Monad.Free (Free (..), liftF)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHCSpecter.Server.Types (ServerState)
import GHCSpecter.UI.Types (UIState)
import GHCSpecter.UI.Types.Event (Event)

-- | Pattern functor for effects of Control DSL.
data ControlF r
  = GetState ((UIState, ServerState) -> r)
  | PutState (UIState, ServerState) r
  | NextEvent (Event -> r)
  | PrintMsg Text r
  | GetCurrentTime (UTCTime -> r)
  | GetLastUpdatedUI (UTCTime -> r)
  | ShouldUpdate Bool r
  | SaveSession r
  deriving (Functor)

type Control = Free ControlF

getState :: Control (UIState, ServerState)
getState = liftF (GetState id)

putState :: (UIState, ServerState) -> Control ()
putState (ui, ss) = liftF (PutState (ui, ss) ())

nextEvent :: Control Event
nextEvent = liftF (NextEvent id)

printMsg :: Text -> Control ()
printMsg txt = liftF (PrintMsg txt ())

getCurrentTime :: Control UTCTime
getCurrentTime = liftF (GetCurrentTime id)

getLastUpdatedUI :: Control UTCTime
getLastUpdatedUI = liftF (GetLastUpdatedUI id)

shouldUpdate :: Bool -> Control ()
shouldUpdate b = liftF (ShouldUpdate b ())

saveSession :: Control ()
saveSession = liftF (SaveSession ())
