module GHCSpecter.Util.GHC
  ( showPpr,
    printPpr,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Driver.Session (DynFlags)
import GHC.Plugins (showSDoc)
import GHC.Utils.Outputable (Outputable (ppr))

showPpr :: (Outputable a) => DynFlags -> a -> String
showPpr dflags = showSDoc dflags . ppr

printPpr :: (Outputable a, MonadIO m) => DynFlags -> a -> m ()
printPpr dflags = liftIO . putStrLn . showPpr dflags
