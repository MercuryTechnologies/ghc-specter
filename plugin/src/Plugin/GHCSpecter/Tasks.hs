{-# LANGUAGE CPP #-}

module Plugin.GHCSpecter.Tasks (
  -- * command set
  type CommandArg,
  CommandSet (..),
  emptyCommandSet,

  -- * tasks per pass/phase
  driverCommands,
  parsedResultActionCommands,
  renamedResultActionCommands,
  preMetaCommands,
  spliceRunActionCommands,
  postMetaCommands,
  rnSpliceCommands,
  typecheckResultActionCommands,
  core2coreCommands,
  prePhaseCommands,
  postPhaseCommands,
) where

import Data.Text (Text)
import GHC.Core.Opt.Monad (CoreM)
import GHC.Driver.Env (Hsc)
#if MIN_VERSION_ghc(9, 4, 0)
#elif MIN_VERSION_ghc(9, 2, 0)
import GHC.Driver.Pipeline (CompPipeline)
#endif
import GHC.Driver.Session (DynFlags)
import GHC.Hs.Extension (GhcRn, GhcTc)
import GHC.Tc.Types (RnM, TcGblEnv (..), TcM)
import GHC.Unit.Module.ModGuts (ModGuts (..))
import GHC.Utils.Outputable (Outputable (..))
import GHCSpecter.Channel.Outbound.Types (ConsoleReply (..))
import Language.Haskell.Syntax.Decls (HsGroup)
#if MIN_VERSION_ghc(9, 6, 0)
import Language.Haskell.Syntax.Expr (HsUntypedSplice, LHsExpr)
#else
import Language.Haskell.Syntax.Expr (HsSplice, LHsExpr)
#endif
import Plugin.GHCSpecter.Tasks.Core2Core (listCore, printCore)
import Plugin.GHCSpecter.Tasks.Typecheck (
  fetchUnqualifiedImports,
  showRenamed,
  showRnSplice,
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

#if MIN_VERSION_ghc(9, 6, 0)
rnSpliceCommands :: HsUntypedSplice GhcRn -> CommandSet RnM
#else
rnSpliceCommands :: HsSplice GhcRn -> CommandSet RnM
#endif
rnSpliceCommands splice =
  CommandSet [(":show-splice", \_ -> showRnSplice splice)]

preMetaCommands :: LHsExpr GhcTc -> CommandSet TcM
preMetaCommands expr =
  CommandSet [(":show-expr", \_ -> showSpliceExpr expr)]

spliceRunActionCommands :: LHsExpr GhcTc -> CommandSet TcM
spliceRunActionCommands expr =
  CommandSet [(":show-expr", \_ -> showSpliceExpr expr)]

postMetaCommands :: (Outputable r) => DynFlags -> r -> CommandSet IO
postMetaCommands dflags result =
  CommandSet [(":show-result", \_ -> pure (showSpliceResult dflags result))]

typecheckResultActionCommands :: TcGblEnv -> CommandSet TcM
typecheckResultActionCommands tc =
  CommandSet [(":unqualified", \_ -> fetchUnqualifiedImports tc)]

core2coreCommands :: ModGuts -> CommandSet CoreM
core2coreCommands guts =
  CommandSet
    [ (":list-core", \_ -> listCore guts)
    , (":print-core", printCore guts)
    ]

#if MIN_VERSION_ghc(9, 4, 0)
prePhaseCommands :: CommandSet IO
#elif MIN_VERSION_ghc(9, 2, 0)
prePhaseCommands :: CommandSet CompPipeline
#endif
prePhaseCommands = emptyCommandSet

#if MIN_VERSION_ghc(9, 4, 0)
postPhaseCommands :: CommandSet IO
#elif MIN_VERSION_ghc(9, 2, 0)
postPhaseCommands :: CommandSet CompPipeline
#endif
postPhaseCommands = emptyCommandSet
