{-# LANGUAGE RecordWildCards #-}

module Toolbox.Worker.Hie
  ( hieWorker,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Iface.Ext.Binary
  ( HieFileResult (..),
    NameCacheUpdater (NCU),
    readHieFile,
  )
import GHC.Iface.Ext.Types (HieFile (..), getAsts)
import GHC.Iface.Ext.Utils (generateReferencesMap)
import GHC.Types.Name.Cache (initNameCache)
import HieDb.Compat
  ( mkSplitUniqSupply,
    moduleName,
    moduleNameString,
    occNameString,
  )
import HieDb.Types (DeclRow (..), DefRow (..), RefRow (..))
import HieDb.Utils (genDefRow, genRefsAndDecls)
import Toolbox.Server.Types
  ( DeclRow' (..),
    DefRow' (..),
    HieState (..),
    ModuleHieInfo (..),
    RefRow' (..),
    ServerState (..),
  )

convertRefRow :: RefRow -> RefRow'
convertRefRow RefRow {..} =
  RefRow'
    { _ref'Src = refSrc
    , _ref'NameOcc = T.pack $ occNameString refNameOcc
    , _ref'NameMod = T.pack $ show refNameMod
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

hieWorker :: TVar ServerState -> FilePath -> IO ()
hieWorker var hiefile = do
  uniq_supply <- mkSplitUniqSupply 'z'
  let nc = initNameCache uniq_supply []
  hieResult <- readHieFile (NCU (\f -> pure $ snd $ f nc)) hiefile
  let hf = hie_file_result hieResult
      modu = hie_module hf
      modName = T.pack $ moduleNameString $ moduleName modu
      asts = hie_asts hf
      refmap = generateReferencesMap $ getAsts asts
      (refs, decls) = genRefsAndDecls "" modu refmap
      defs = genDefRow "" modu refmap
  atomically $
    modifyTVar' var $ \ss ->
      let HieState hieModMap = _serverHieState ss
          modHie =
            ModuleHieInfo
              { _modHieRefs = fmap convertRefRow refs
              , _modHieDecls = fmap convertDeclRow decls
              , _modHieDefs = fmap convertDefRow defs
              }
          hieModMap' = M.insert modName modHie hieModMap
       in ss {_serverHieState = HieState hieModMap'}
