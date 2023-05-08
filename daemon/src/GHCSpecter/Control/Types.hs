{-# LANGUAGE GADTs #-}

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
) where

import Control.Concurrent.STM (TVar)
import Control.Monad.Indexed (IxFunctor (..))
import Control.Monad.Indexed.Free (IxFree (..))
import Control.Monad.Indexed.Free.Class (iliftFree)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHCSpecter.Channel.Inbound.Types (Request)
import GHCSpecter.Graphics.DSL (EventMap, Scene)
import GHCSpecter.Server.Types (ServerState)
import GHCSpecter.UI.Types (UIState)
import GHCSpecter.UI.Types.Event (Event)

-- | Pattern functor for effects of Control DSL.
-- TODO: once commit (atomic state update) and refresh frame are cleared,
-- then we can guarantee the atomicity of state update inside frame with
-- type index, so no need for listing complex get\/put\/modify cases.
data ControlF e e' r where
  GetUI :: (UIState -> r) -> ControlF e e r
  PutUI :: UIState -> r -> ControlF e e r
  ModifyUI :: (UIState -> UIState) -> r -> ControlF e e r
  GetSS :: (ServerState -> r) -> ControlF e e r
  PutSS :: ServerState -> r -> ControlF e e r
  ModifySS :: (ServerState -> ServerState) -> r -> ControlF e e r
  ModifyUISS :: ((UIState, ServerState) -> (UIState, ServerState)) -> r -> ControlF e e r
  ModifyAndReturn ::
    ((UIState, ServerState) -> (UIState, ServerState)) ->
    ((UIState, ServerState) -> r) ->
    ControlF e e r
  ModifyAndReturnBoth ::
    ((UIState, ServerState) -> (UIState, ServerState)) ->
    (((UIState, ServerState), (UIState, ServerState)) -> r) ->
    ControlF e e r
  HitScene ::
    (Double, Double) ->
    (Maybe (EventMap e) -> r) ->
    ControlF e e r
  GetScene ::
    Text ->
    (Maybe (EventMap e) -> r) ->
    ControlF e e r
  AddToStage ::
    Scene () ->
    r ->
    ControlF e e r
  SendRequest :: Request -> r -> ControlF e e r
  NextEvent :: (Event -> r) -> ControlF e e r
  PrintMsg :: Text -> r -> ControlF e e r
  GetCurrentTime :: (UTCTime -> r) -> ControlF e e r
  GetLastUpdatedUI :: (UTCTime -> r) -> ControlF e e r
  ShouldUpdate :: Bool -> r -> ControlF e e r
  SaveSession :: r -> ControlF e e r
  Refresh :: r -> ControlF e e r
  RefreshUIAfter :: Double -> r -> ControlF e e r
  AsyncWork :: (TVar ServerState -> IO ()) -> r -> ControlF e e r

instance IxFunctor ControlF where
  imap f (GetUI cont) = GetUI (f . cont)
  imap f (PutUI x next) = PutUI x (f next)
  imap f (ModifyUI g next) = ModifyUI g (f next)
  imap f (GetSS cont) = GetSS (f . cont)
  imap f (PutSS x next) = PutSS x (f next)
  imap f (ModifySS g next) = ModifySS g (f next)
  imap f (ModifyUISS g next) = ModifyUISS g (f next)
  imap f (ModifyAndReturn g cont) = ModifyAndReturn g (f . cont)
  imap f (ModifyAndReturnBoth g cont) = ModifyAndReturnBoth g (f . cont)
  imap f (HitScene x cont) = HitScene x (f . cont)
  imap f (GetScene x cont) = GetScene x (f . cont)
  imap f (AddToStage x next) = AddToStage x (f next)
  imap f (SendRequest x next) = SendRequest x (f next)
  imap f (NextEvent cont) = NextEvent (f . cont)
  imap f (PrintMsg x next) = PrintMsg x (f next)
  imap f (GetCurrentTime cont) = GetCurrentTime (f . cont)
  imap f (GetLastUpdatedUI cont) = GetLastUpdatedUI (f . cont)
  imap f (ShouldUpdate x next) = ShouldUpdate x (f next)
  imap f (SaveSession next) = SaveSession (f next)
  imap f (Refresh next) = Refresh (f next)
  imap f (RefreshUIAfter x next) = RefreshUIAfter x (f next)
  imap f (AsyncWork g next) = AsyncWork g (f next)

instance Functor (ControlF () ()) where
  fmap = imap

type Control' = IxFree ControlF

type Control e = Control' e e

-- use this as synonym
liftF :: ControlF e e a -> Control e a
liftF = iliftFree

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
  Control e (Maybe (EventMap e))
hitScene xy = liftF (HitScene xy id)

-- | Get scene event map by name
getScene ::
  Text ->
  Control e (Maybe (EventMap e))
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
