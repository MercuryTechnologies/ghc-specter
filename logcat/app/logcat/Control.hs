module Control (
  ControlF (..),
  Control,
  modifyState,
  nextEvent,
  stepControl,
) where

import Control.Concurrent.MVar (MVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Types (CEvent (..), LogcatState)

data ControlF r
  = ModifyState (LogcatState -> LogcatState) r
  | NextEvent (CEvent -> r)
  deriving (Functor)

type Control = Free ControlF

modifyState :: (LogcatState -> LogcatState) -> Control ()
modifyState upd = liftF (ModifyState upd ())

nextEvent :: Control CEvent
nextEvent = liftF (NextEvent id)

stepControl :: Control r -> ReaderT (MVar CEvent, TVar LogcatState) IO (Either (Control r) r)
stepControl (Pure r) = pure (Right r)
stepControl (Free (ModifyState upd next)) = do
  liftIO $ putStrLn "modifyState"
  (_, ref) <- ask
  liftIO $ atomically $ modifyTVar' ref upd
  pure (Left next)
stepControl (Free (NextEvent cont)) = do
  liftIO $ putStrLn "nextEvent"
  (lock, _) <- ask
  ev <- liftIO $ takeMVar lock
  liftIO $ print ev
  pure (Left (cont ev))
