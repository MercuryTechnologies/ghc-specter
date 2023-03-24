{-# LANGUAGE RecordWildCards #-}

module GHCSpecter.Worker.Hie
  ( hieWorker,
    moduleSourceWorker,
  )
where

import Control.Concurrent.STM
  ( TQueue,
    TVar,
    atomically,
    modifyTVar',
    writeTQueue,
  )
import Control.Lens ((%~), (.~))
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.IO qualified as TIO
import GHC.Iface.Ext.Binary
  ( HieFileResult (..),
    readHieFile,
  )
import GHC.Iface.Ext.Types (HieFile (..), getAsts)
import GHC.Iface.Ext.Utils (generateReferencesMap)
import GHC.Types.Name.Cache (initNameCache)
import GHCSpecter.Channel.Common.Types (ModuleName)
import GHCSpecter.Data.GHC.Hie
  ( DeclRow' (..),
    DefRow' (..),
    HasModuleHieInfo (..),
    RefRow' (..),
    emptyModuleHieInfo,
  )
import GHCSpecter.Server.Types
  ( HasHieState (..),
    HasServerState (..),
    ServerState (..),
  )
import GHCSpecter.Worker.CallGraph qualified as CallGraph


hieWorker :: TVar ServerState -> TQueue (IO ()) -> FilePath -> IO ()
hieWorker ssRef workQ hiefile = do
  pure ()

moduleSourceWorker :: TVar ServerState -> Map ModuleName FilePath -> IO ()
moduleSourceWorker ssRef modSrcs = do
  for_ (M.toList modSrcs) $ \(modu, srcFile) -> do
    src <- TIO.readFile srcFile
    let update Nothing = Just ((modHieSource .~ src) emptyModuleHieInfo)
        update (Just modHie) = Just ((modHieSource .~ src) modHie)
    atomically $
      modifyTVar' ssRef (serverHieState . hieModuleMap %~ M.alter update modu)
