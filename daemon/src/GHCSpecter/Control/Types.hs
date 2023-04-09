module GHCSpecter.Control.Types (
  -- * eDSL Types
  ControlF (..),
  Control,

  -- * Primitive operations of eDSL
  getUI,
  putUI,
  modifyUI,
  getSS,
  putSS,
  modifySS,
  modifyUISS,
  modifyAndReturn,
  modifyAndReturnBoth,
  sendRequest,
  nextEvent,
  printMsg,
  getCurrentTime,
  getLastUpdatedUI,
  refresh,
  refreshUIAfter,
  shouldUpdate,
  saveSession,
  asyncWork,
) where

import Control.Concurrent.STM (TVar)
import Control.Monad.Free (Free (..), liftF)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHCSpecter.Channel.Inbound.Types (Request)
import GHCSpecter.Server.Types (ServerState)
import GHCSpecter.UI.Types (UIState)
import GHCSpecter.UI.Types.Event (Event)

-- | Pattern functor for effects of Control DSL.
-- TODO: remove PutUI and PutSS in the end
-- to guarantee atomic updates.
data ControlF r
  = GetUI (UIState -> r)
  | PutUI UIState r
  | ModifyUI (UIState -> UIState) r
  | GetSS (ServerState -> r)
  | PutSS ServerState r
  | ModifySS (ServerState -> ServerState) r
  | ModifyUISS ((UIState, ServerState) -> (UIState, ServerState)) r
  | ModifyAndReturn ((UIState, ServerState) -> (UIState, ServerState)) ((UIState, ServerState) -> r)
  | ModifyAndReturnBoth
      ((UIState, ServerState) -> (UIState, ServerState))
      (((UIState, ServerState), (UIState, ServerState)) -> r)
  | SendRequest Request r
  | NextEvent (Event -> r)
  | PrintMsg Text r
  | GetCurrentTime (UTCTime -> r)
  | GetLastUpdatedUI (UTCTime -> r)
  | ShouldUpdate Bool r
  | SaveSession r
  | Refresh r
  | RefreshUIAfter Double r
  | AsyncWork (TVar ServerState -> IO ()) r
  deriving (Functor)

type Control = Free ControlF

getUI :: Control UIState
getUI = liftF (GetUI id)

putUI :: UIState -> Control ()
putUI ui = liftF (PutUI ui ())

modifyUI :: (UIState -> UIState) -> Control ()
modifyUI upd = liftF (ModifyUI upd ())

getSS :: Control ServerState
getSS = liftF (GetSS id)

putSS :: ServerState -> Control ()
putSS ss = liftF (PutSS ss ())

modifySS :: (ServerState -> ServerState) -> Control ()
modifySS upd = liftF (ModifySS upd ())

modifyUISS :: ((UIState, ServerState) -> (UIState, ServerState)) -> Control ()
modifyUISS upd = liftF (ModifyUISS upd ())

modifyAndReturn :: ((UIState, ServerState) -> (UIState, ServerState)) -> Control (UIState, ServerState)
modifyAndReturn upd = liftF (ModifyAndReturn upd id)

modifyAndReturnBoth ::
  ((UIState, ServerState) -> (UIState, ServerState)) ->
  Control ((UIState, ServerState), (UIState, ServerState))
modifyAndReturnBoth upd = liftF (ModifyAndReturnBoth upd id)

sendRequest :: Request -> Control ()
sendRequest b = liftF (SendRequest b ())

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

-- | Perform refresh, which is UI backend dependent.
refresh :: Control ()
refresh = liftF (Refresh ())

-- | Reserve calling refresh after nSec seconds. Note that this does not perform refresh action,
-- but create an event for refresh that will trigger the @refreshUI@ action.
refreshUIAfter :: Double -> Control ()
refreshUIAfter nSec = liftF (RefreshUIAfter nSec ())

asyncWork :: (TVar ServerState -> IO ()) -> Control ()
asyncWork w = liftF (AsyncWork w ())
