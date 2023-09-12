{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -w #-}

module GHCSpecter.Control.Types
  ( -- * tiny indexed free monad
    IFree (..),
    liftF,

    -- * eDSL Types
    ControlF (..),
    Control',
    Control,
  )
where

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
import GHCSpecter.UI.Types.Event
  ( Event,
    UserEvent,
  )

-- NOTE: Super-compact version as we need. Currently, the most of the code
-- is a simplified version of the packages: free, indexed, and indexed-free [1, 2, 3]
-- Later on, we will explore a few ideas (in particular, delim continuation based)
-- on how to do indexed-state-based control process.

-- [1] https://hackage.haskell.org/package/free
-- [2] https://hackage.haskell.org/package/indexed
-- [3] https://hackage.haskell.org/package/indexed-free

class IFunctor f where
  ifmap :: (a -> b) -> f i j a -> f i j b

class IFunctor m => IApplicative m where
  ipure :: a -> m i i a
  iap :: m i j (a -> b) -> m j k a -> m i k b

class IApplicative m => IMonad m where
  ibind :: (a -> m i j b) -> m k i a -> m k j b

data IFree f i j a where
  IPure :: a -> IFree f i i a
  IFree :: f i j (IFree f j k a) -> IFree f i k a

-- use this as synonym
liftF :: (IFunctor f) => f i j a -> IFree f i j a
liftF f = IFree (ifmap ipure f)

instance IFunctor f => IFunctor (IFree f) where
  ifmap g (IPure x) = IPure (g x)
  ifmap g (IFree fx) = IFree (ifmap (ifmap g) fx)

instance IFunctor f => Functor (IFree f i i) where
  fmap = ifmap

instance IFunctor f => IApplicative (IFree f) where
  ipure = IPure

  iap (IPure g) mx = ifmap g mx
  iap (IFree fmg) mx = IFree (ifmap (\mg -> iap mg mx) fmg)

instance IFunctor f => IMonad (IFree f) where
  ibind g (IPure x) = g x
  ibind g (IFree fmx) = IFree (ifmap (ibind g) fmx)

instance IFunctor f => Applicative (IFree f i i) where
  pure = ipure
  (<*>) = iap

instance IFunctor f => Monad (IFree f i i) where
  (>>=) = flip ibind

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
    (Maybe (EventMap UserEvent) -> r) ->
    ControlF e e r
  GetScene ::
    Text ->
    (Maybe (EventMap UserEvent) -> r) ->
    ControlF e e r
  AddToStage ::
    Scene () ->
    r ->
    ControlF e e r
  ScrollDownConsoleToEnd :: r -> ControlF e e r
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

instance IFunctor ControlF where
  ifmap f (GetUI cont) = GetUI (f . cont)
  ifmap f (PutUI x next) = PutUI x (f next)
  ifmap f (ModifyUI g next) = ModifyUI g (f next)
  ifmap f (GetSS cont) = GetSS (f . cont)
  ifmap f (PutSS x next) = PutSS x (f next)
  ifmap f (ModifySS g next) = ModifySS g (f next)
  ifmap f (ModifyUISS g next) = ModifyUISS g (f next)
  ifmap f (ModifyAndReturn g cont) = ModifyAndReturn g (f . cont)
  ifmap f (ModifyAndReturnBoth g cont) = ModifyAndReturnBoth g (f . cont)
  ifmap f (HitScene x cont) = HitScene x (f . cont)
  ifmap f (GetScene x cont) = GetScene x (f . cont)
  ifmap f (AddToStage x next) = AddToStage x (f next)
  ifmap f (ScrollDownConsoleToEnd next) = ScrollDownConsoleToEnd (f next)
  ifmap f (SendRequest x next) = SendRequest x (f next)
  ifmap f (NextEvent cont) = NextEvent (f . cont)
  ifmap f (PrintMsg x next) = PrintMsg x (f next)
  ifmap f (GetCurrentTime cont) = GetCurrentTime (f . cont)
  ifmap f (GetLastUpdatedUI cont) = GetLastUpdatedUI (f . cont)
  ifmap f (ShouldUpdate x next) = ShouldUpdate x (f next)
  ifmap f (SaveSession next) = SaveSession (f next)
  ifmap f (Refresh next) = Refresh (f next)
  ifmap f (RefreshUIAfter x next) = RefreshUIAfter x (f next)
  ifmap f (AsyncWork g next) = AsyncWork g (f next)

-- instance Functor (ControlF e e) where
--   fmap = imap

type Control' = IFree ControlF

type Control e = Control' e e
