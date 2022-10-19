module Plugin.GHCSpecter.Task
  ( -- * command set
    type CommandArg,
    CommandSet (..),
    emptyCommandSet,

    -- * tasks per pass/phase
    driverCommands,
    parsedResultActionCommands,
    renamedResultActionCommands,
    preRunMetaCommands,
    spliceRunActionCommands,
    postRunMetaCommands,
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
import GHC.Hs.Extension (GhcRn, GhcTc)
import GHC.Tc.Types (TcGblEnv (..), TcM)
import GHC.Unit.Module.ModGuts (ModGuts (..))
import GHC.Utils.Outputable (Outputable (..))
import GHCSpecter.Channel.Outbound.Types (ConsoleReply)
import Language.Haskell.Syntax.Decls (HsGroup)
import Language.Haskell.Syntax.Expr (LHsExpr)
import Plugin.GHCSpecter.Task.Core2Core (listCore, printCore)
import Plugin.GHCSpecter.Task.Typecheck
  ( fetchUnqualifiedImports,
    showRenamed,
    showSpliceExpr,
    showSpliceResult,
  )

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

renamedResultActionCommands :: HsGroup GhcRn -> CommandSet TcM
renamedResultActionCommands grp =
  CommandSet [(":show-renamed", \_ -> showRenamed grp)]

preRunMetaCommands :: LHsExpr GhcTc -> CommandSet TcM
preRunMetaCommands expr =
  CommandSet [(":show-expr", \_ -> showSpliceExpr expr)]

spliceRunActionCommands :: LHsExpr GhcTc -> CommandSet TcM
spliceRunActionCommands expr =
  CommandSet [(":show-expr", \_ -> showSpliceExpr expr)]

postRunMetaCommands :: (Outputable r) => r -> CommandSet TcM
postRunMetaCommands result =
  CommandSet [(":show-result", \_ -> showSpliceResult result)]

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
