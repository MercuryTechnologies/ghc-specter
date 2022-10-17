module Plugin.GHCSpecter.Task
  ( -- * command set
    type CommandArg,
    CommandSet (..),
    emptyCommandSet,

    -- * tasks per pass/phase
    driverCommands,
    parsedResultActionCommands,
    renamedResultActionCommands,
    spliceRunActionCommands,
    typecheckResultActionCommands,
    core2coreCommands,
    prePhaseCommands,
    postPhaseCommands,
  )
where

import Data.Text (Text)
import GHC.Core.Opt.Monad (CoreM)
import GHC.Driver.Env (Hsc)
import GHC.Driver.Pipeline (CompPipeline)
import GHC.Tc.Types (TcGblEnv (..), TcM)
import GHC.Unit.Module.ModGuts (ModGuts (..))
import GHCSpecter.Channel.Outbound.Types (ConsoleReply)
import Plugin.GHCSpecter.Task.Core2Core (listCore, printCore)
import Plugin.GHCSpecter.Task.Typecheck (fetchUnqualifiedImports)

type CommandArg = Text

-- | a list of available commands at a given breakpoint
newtype CommandSet m = CommandSet
  { unCommandSet :: [(Text, [CommandArg] -> m ConsoleReply)]
  -- ^ each item: command name, command action
  -- command action takes additional arguments.
  }

emptyCommandSet :: CommandSet m
emptyCommandSet = CommandSet []

driverCommands :: CommandSet IO
driverCommands = emptyCommandSet

parsedResultActionCommands :: CommandSet Hsc
parsedResultActionCommands = emptyCommandSet

renamedResultActionCommands :: CommandSet TcM
renamedResultActionCommands = emptyCommandSet

spliceRunActionCommands :: CommandSet TcM
spliceRunActionCommands = emptyCommandSet

typecheckResultActionCommands :: TcGblEnv -> CommandSet TcM
typecheckResultActionCommands tc =
  CommandSet [(":unqualified", \_ -> fetchUnqualifiedImports tc)]

core2coreCommands :: ModGuts -> CommandSet CoreM
core2coreCommands guts =
  CommandSet
    [ (":list-core", \_ -> listCore guts)
    , (":print-core", printCore guts)
    ]

prePhaseCommands :: CommandSet CompPipeline
prePhaseCommands = emptyCommandSet

postPhaseCommands :: CommandSet CompPipeline
postPhaseCommands = emptyCommandSet
