module GHCSpecter.Control.DSL
  ( -- * Primitive operations of eDSL
    getUI,
    putUI,
    modifyUI,
    getSS,
    putSS,
    modifySS,
    modifyUISS,
    modifyAndReturn,
    modifyAndReturnBoth,
    hitScene,
    getScene,
    addToStage,
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
  )
where

import Control.Concurrent.STM (TVar)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHCSpecter.Channel.Inbound.Types (Request)
import GHCSpecter.Control.Types
  ( Control,
    ControlF (..),
    liftF,
  )
import GHCSpecter.Graphics.DSL (EventMap, Scene)
import GHCSpecter.Server.Types (ServerState)
import GHCSpecter.UI.Types (UIState)
import GHCSpecter.UI.Types.Event (Event, UserEvent)

getUI :: Control e UIState
getUI = liftF (GetUI id)

putUI :: UIState -> Control e ()
putUI ui = liftF (PutUI ui ())

modifyUI :: (UIState -> UIState) -> Control e ()
modifyUI upd = liftF (ModifyUI upd ())

getSS :: Control e ServerState
getSS = liftF (GetSS id)

putSS :: ServerState -> Control e ()
putSS ss = liftF (PutSS ss ())

modifySS :: (ServerState -> ServerState) -> Control e ()
modifySS upd = liftF (ModifySS upd ())

modifyUISS :: ((UIState, ServerState) -> (UIState, ServerState)) -> Control e ()
modifyUISS upd = liftF (ModifyUISS upd ())

modifyAndReturn ::
  ((UIState, ServerState) -> (UIState, ServerState)) ->
  Control e (UIState, ServerState)
modifyAndReturn upd = liftF (ModifyAndReturn upd id)

modifyAndReturnBoth ::
  ((UIState, ServerState) -> (UIState, ServerState)) ->
  Control e ((UIState, ServerState), (UIState, ServerState))
modifyAndReturnBoth upd = liftF (ModifyAndReturnBoth upd id)

-- | Hit test for canvas coordinate. Return the eventmap of hit scene.
hitScene ::
  (Double, Double) ->
  Control e (Maybe (EventMap UserEvent))
hitScene xy = liftF (HitScene xy id)

-- | Get scene event map by name
getScene ::
  Text ->
  Control e (Maybe (EventMap UserEvent))
getScene name = liftF (GetScene name id)

-- | add a scene to the stage
addToStage ::
  Scene () ->
  Control e ()
addToStage scene = liftF (AddToStage scene ())

sendRequest :: Request -> Control e ()
sendRequest b = liftF (SendRequest b ())

nextEvent :: Control e Event
nextEvent = liftF (NextEvent id)

printMsg :: Text -> Control e ()
printMsg txt = liftF (PrintMsg txt ())

getCurrentTime :: Control e UTCTime
getCurrentTime = liftF (GetCurrentTime id)

getLastUpdatedUI :: Control e UTCTime
getLastUpdatedUI = liftF (GetLastUpdatedUI id)

shouldUpdate :: Bool -> Control e ()
shouldUpdate b = liftF (ShouldUpdate b ())

saveSession :: Control e ()
saveSession = liftF (SaveSession ())

-- | Perform refresh, which is UI backend dependent.
refresh :: Control e ()
refresh = liftF (Refresh ())

-- | Reserve calling refresh after nSec seconds. Note that this does not perform refresh action,
-- but create an event for refresh that will trigger the @refreshUI@ action.
refreshUIAfter :: Double -> Control e ()
refreshUIAfter nSec = liftF (RefreshUIAfter nSec ())

asyncWork :: (TVar ServerState -> IO ()) -> Control e ()
asyncWork w = liftF (AsyncWork w ())
