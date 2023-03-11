module Control (
  ControlF (..),
  Control,
  getState,
  putState,
  nextEvent,
  stepControl,
) where

import Control.Concurrent.MVar (MVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Types (CEvent (..), LogcatState)

data ControlF r
  = GetState (LogcatState -> r)
  | PutState LogcatState r
  | NextEvent (CEvent -> r)
  deriving (Functor)

type Control = Free ControlF

getState :: Control LogcatState
getState = liftF (GetState id)

putState :: LogcatState -> Control ()
putState s = liftF (PutState s ())

nextEvent :: Control CEvent
nextEvent = liftF (NextEvent id)

stepControl :: Control r -> ReaderT (MVar CEvent, TVar LogcatState) IO (Either (Control r) r)
stepControl (Pure r) = pure (Right r)
stepControl (Free (GetState cont)) = do
  liftIO $ putStrLn "getState"
  (_, ref) <- ask
  s <- liftIO $ atomically $ readTVar ref
  pure (Left (cont s))
stepControl (Free (PutState s next)) = do
  liftIO $ putStrLn "putState"
  (_, ref) <- ask
  liftIO $ atomically $ writeTVar ref s
  pure (Left next)
stepControl (Free (NextEvent cont)) = do
  liftIO $ putStrLn "nextEvent"
  (lock, _) <- ask
  ev <- liftIO $ takeMVar lock
  liftIO $ print ev
  pure (Left (cont ev))
