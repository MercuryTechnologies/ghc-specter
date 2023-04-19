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
import GHCSpecter.Server.Types (ServerState)
import GHCSpecter.UI.Types (UIState)
import GHCSpecter.UI.Types.Event (Event)

-- | Pattern functor for effects of Control DSL.
-- TODO: remove PutUI and PutSS in the end
-- to guarantee atomic updates.
data ControlF i j r where
  GetUI :: (UIState -> r) -> ControlF i i r
  PutUI :: UIState -> r -> ControlF i i r
  ModifyUI :: (UIState -> UIState) -> r -> ControlF i i r
  GetSS :: (ServerState -> r) -> ControlF i i r
  PutSS :: ServerState -> r -> ControlF i i r
  ModifySS :: (ServerState -> ServerState) -> r -> ControlF i i r
  ModifyUISS :: ((UIState, ServerState) -> (UIState, ServerState)) -> r -> ControlF i i r
  ModifyAndReturn ::
    ((UIState, ServerState) -> (UIState, ServerState)) ->
    ((UIState, ServerState) -> r) ->
    ControlF i i r
  ModifyAndReturnBoth ::
    ((UIState, ServerState) -> (UIState, ServerState)) ->
    (((UIState, ServerState), (UIState, ServerState)) -> r) ->
    ControlF i i r
  SendRequest :: Request -> r -> ControlF i i r
  NextEvent :: (Event -> r) -> ControlF i i r
  PrintMsg :: Text -> r -> ControlF i i r
  GetCurrentTime :: (UTCTime -> r) -> ControlF i i r
  GetLastUpdatedUI :: (UTCTime -> r) -> ControlF i i r
  ShouldUpdate :: Bool -> r -> ControlF i i r
  SaveSession :: r -> ControlF i i r
  Refresh :: r -> ControlF i i r
  RefreshUIAfter :: Double -> r -> ControlF i i r
  AsyncWork :: (TVar ServerState -> IO ()) -> r -> ControlF i i r

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

type Control = Control' () ()

-- use this as synonym
liftF :: ControlF () () a -> Control a
liftF = iliftFree

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
