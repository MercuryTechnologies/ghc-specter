module GHCSpecter.Control.Runner
  ( type Runner,
    stepControl,
    stepControlUpToEvent,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
  ( TChan,
    TVar,
    atomically,
    readTVar,
    writeTChan,
    writeTVar,
  )
import Control.Lens ((.~), (^.), _1)
import Control.Monad.Extra (loopM)
import Control.Monad.Free (Free (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock qualified as Clock
import GHCSpecter.Control.Types
  ( ControlF (..),
    type Control,
  )
import GHCSpecter.Server.Types (ServerState)
import GHCSpecter.UI.Types
  ( HasUIState (..),
    UIState (..),
  )
import GHCSpecter.UI.Types.Event
  ( BackgroundEvent (..),
    Event (..),
  )
import System.IO (IOMode (..), withFile)
import System.IO.Unsafe (unsafePerformIO)

-- TODO: remove this
tempRef :: IORef Int
tempRef = unsafePerformIO (newIORef 0)
{-# NOINLINE tempRef #-}

type Runner = ReaderT (TVar UIState, TVar ServerState, TChan BackgroundEvent) IO

getState' :: Runner (UIState, ServerState)
getState' = do
  (uiRef, ssRef, _) <- ask
  liftIO $
    atomically $
      (,) <$> readTVar uiRef <*> readTVar ssRef

putState' :: (UIState, ServerState) -> Runner ()
putState' (ui, ss) = do
  (uiRef, ssRef, _) <- ask
  liftIO $
    atomically $ do
      writeTVar uiRef ui
      writeTVar ssRef ss

modifyState' :: ((UIState, ServerState) -> (UIState, ServerState)) -> Runner ()
modifyState' f = do
  s <- getState'
  let s' = f s
  s' `seq` putState' s'

{-

Note [Control Loops]
~~~~~~~~~~~~~~~~~~~

Control monad is embedded DSL (eDSL) which describe the program logic of the ghc-specter
daemon. It is based on Free monad over a pattern functor that defines the allowed effects
of the program logic.

Among the effects, NextEvent is special since, at that step, the control runner yields
its operation to the underlying Widget rendering engine or message receiver until a new
event comes. Therefore, the control loop is defined in terms of 2-layered nested loops,
where the outer loop is to run the event poking step interleaved with inner loop operations,
and the inner loop is to process non-event-poking steps.

-}

-- | A single primitive step for the inner loop. See Note [Control Loops].
stepControl ::
  Control r ->
  -- | What the result means:
  -- Left _: continuation in the inner loop.
  -- Right (Left _): continuation that waits for a new event in the outer loop
  -- Right (Right _): final result as the business logic reaches its end.
  -- TODO: Use more descriptive custom types.
  Runner
    ( Either
        (Control r)
        ( Either
            (Event -> Control r)
            r
        )
    )
stepControl (Pure r) = pure (Right (Right r))
stepControl (Free (GetState cont)) = do
  (ui, ss) <- getState'
  pure (Left (cont (ui, ss)))
stepControl (Free (PutState (ui, ss) next)) = do
  putState' (ui, ss)
  pure (Left next)
stepControl (Free (NextEvent cont)) =
  pure (Right (Left cont))
stepControl (Free (PrintMsg txt next)) = do
  liftIO $ do
    n <- readIORef tempRef
    modifyIORef' tempRef (+ 1)
    TIO.putStrLn $ (T.pack (show n) <> " : " <> txt)
  pure (Left next)
stepControl (Free (GetCurrentTime cont)) = do
  now <- liftIO Clock.getCurrentTime
  pure (Left (cont now))
stepControl (Free (GetLastUpdatedUI cont)) = do
  lastUpdatedUI <- (^. _1 . uiLastUpdated) <$> getState'
  pure (Left (cont lastUpdatedUI))
stepControl (Free (ShouldUpdate b next)) = do
  modifyState' (_1 . uiShouldUpdate .~ b)
  pure (Left next)
stepControl (Free (SaveSession next)) = do
  (_, ss) <- getState'
  -- TODO: use asynchronous worker
  liftIO $
    withFile "session.json" WriteMode $ \h ->
      BL.hPutStr h (encode ss)
  pure (Left next)
stepControl (Free (RefreshUIAfter nSec next)) = do
  (_, _, chanBkg) <- ask
  liftIO $ do
    threadDelay (floor (nSec * 1_000_000))
    atomically $ writeTChan chanBkg RefreshUI
  pure (Left next)

-- | The inner loop described in the Note [Control Loops].
stepControlUpToEvent ::
  Event ->
  (Event -> Control r) ->
  Runner (Either (Event -> Control r) r)
stepControlUpToEvent ev cont0 = loopM stepControl (cont0 ev)
