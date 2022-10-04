module Plugin.GHCSpecter.Task.PrintCore
  ( printCore,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Core.Opt.Monad (CoreM, getDynFlags)
import GHC.Unit.Module.ModGuts (ModGuts (..))
import GHCSpecter.Util.GHC (showPpr)

printCore :: ModGuts -> CoreM Text
printCore guts = do
  dflags <- getDynFlags
  let txt = T.pack $ showPpr dflags (mg_binds guts)
  pure txt
