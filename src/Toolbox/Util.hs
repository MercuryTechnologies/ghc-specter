module Toolbox.Util
  ( showPpr,
    showPprDebug,
    printPpr,
    printPprDebug,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Driver.Session (DynFlags)
import GHC.Plugins (showSDoc, showSDocDebug)
import GHC.Utils.Outputable (Outputable (ppr))

showPpr :: (Outputable a) => DynFlags -> a -> String
showPpr dflags = showSDoc dflags . ppr

showPprDebug :: (Outputable a) => DynFlags -> a -> String
showPprDebug dflags = showSDocDebug dflags . ppr

printPpr :: (Outputable a, MonadIO m) => DynFlags -> a -> m ()
printPpr dflags = liftIO . putStrLn . showPpr dflags

printPprDebug :: (Outputable a, MonadIO m) => DynFlags -> a -> m ()
printPprDebug dflags = liftIO . putStrLn . showPprDebug dflags
