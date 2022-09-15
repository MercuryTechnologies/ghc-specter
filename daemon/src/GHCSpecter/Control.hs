module GHCSpecter.Control
  ( control,
    runControl,
  )
where

import Control.Monad.Extra (loopM)
import Control.Monad.Free (Free (..), liftF)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import GHCSpecter.Server.Types (ServerState (..))
import GHCSpecter.UI.Types (UIState (..))
import GHCSpecter.UI.Types.Event (Event)

data ControlF r
  = CommitUI UIState r
  | CommitServer ServerState r
  | NextEvent (Event -> r)
  | PrintMsg Text r
  deriving (Functor)

type Control = Free ControlF

commitUI :: UIState -> Control ()
commitUI ui = liftF (CommitUI ui ())

commitServer :: ServerState -> Control ()
commitServer ss = liftF (CommitServer ss ())

nextEvent :: Control Event
nextEvent = liftF (NextEvent id)

printMsg :: Text -> Control ()
printMsg txt = liftF (PrintMsg txt ())

-- | step interpretation
stepControl :: Control r -> IO (Either (Control r) r)
stepControl (Pure r) = pure (Right r)
stepControl (Free (CommitUI _ next)) = pure (Left next)
stepControl (Free (CommitServer _ next)) = pure (Left next)
stepControl (Free (NextEvent cont)) = pure (Left (cont undefined))
stepControl (Free (PrintMsg txt next)) = TIO.putStrLn txt >> pure (Left next)

runControl :: Control r -> IO r
runControl action = loopM stepControl action

control :: Control ()
control = do
  commitUI undefined
  printMsg "message1"
  commitServer undefined
  printMsg "message2"
  _ <- nextEvent
  printMsg "message3"
