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
import HieDb.Compat
  ( moduleName,
    moduleNameString,
    occNameString,
  )
import HieDb.Types (DeclRow (..), DefRow (..), RefRow (..))
import HieDb.Utils (genDefRow, genRefsAndDecls)

convertRefRow :: RefRow -> RefRow'
convertRefRow RefRow {..} =
  RefRow'
    { _ref'Src = refSrc
    , _ref'NameOcc = T.pack $ occNameString refNameOcc
    , _ref'NameMod = T.pack $ moduleNameString refNameMod
    , _ref'NameUnit = T.pack $ show refNameUnit
    , _ref'SLine = refSLine
    , _ref'SCol = refSCol
    , _ref'ELine = refELine
    , _ref'ECol = refECol
    }

convertDeclRow :: DeclRow -> DeclRow'
convertDeclRow DeclRow {..} =
  DeclRow'
    { _decl'Src = declSrc
    , _decl'NameOcc = T.pack $ occNameString declNameOcc
    , _decl'SLine = declSLine
    , _decl'SCol = declSCol
    , _decl'ELine = declELine
    , _decl'ECol = declECol
    , _decl'Root = declRoot
    }

convertDefRow :: DefRow -> DefRow'
convertDefRow DefRow {..} =
  DefRow'
    { _def'Src = defSrc
    , _def'NameOcc = T.pack $ occNameString defNameOcc
    , _def'SLine = defSLine
    , _def'SCol = defSCol
    , _def'ELine = defELine
    , _def'ECol = defECol
    }

hieWorker :: TVar ServerState -> TQueue (IO ()) -> FilePath -> IO ()
hieWorker ssRef workQ hiefile = do
  nc <- initNameCache 'z' []
  hieResult <- readHieFile nc hiefile
  let hf = hie_file_result hieResult
      src = decodeUtf8With (\_ _ -> Just ' ') $ hie_hs_src hf
      modu = hie_module hf
      modName = T.pack $ moduleNameString $ moduleName modu
      asts = hie_asts hf
      refmap = generateReferencesMap $ getAsts asts
      (refs, decls) = genRefsAndDecls "" modu refmap
      defs = genDefRow "" modu refmap
      modHie =
        (modHieRefs .~ fmap convertRefRow refs)
          . (modHieDecls .~ fmap convertDeclRow decls)
          . (modHieDefs .~ fmap convertDefRow defs)
          . (modHieSource .~ src)
          $ emptyModuleHieInfo
      callGraphWork = CallGraph.worker ssRef modName modHie
  atomically $ do
    modifyTVar' ssRef $
      serverHieState . hieModuleMap
        %~ M.insert modName modHie
    writeTQueue workQ callGraphWork

moduleSourceWorker :: TVar ServerState -> Map ModuleName FilePath -> IO ()
moduleSourceWorker ssRef modSrcs = do
  for_ (M.toList modSrcs) $ \(modu, srcFile) -> do
    src <- TIO.readFile srcFile
    let update Nothing = Just ((modHieSource .~ src) emptyModuleHieInfo)
        update (Just modHie) = Just ((modHieSource .~ src) modHie)
    atomically $
      modifyTVar' ssRef (serverHieState . hieModuleMap %~ M.alter update modu)
