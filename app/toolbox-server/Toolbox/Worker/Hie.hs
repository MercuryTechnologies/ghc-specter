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
    { ref'Src = refSrc
    , ref'NameOcc = T.pack $ occNameString refNameOcc
    , ref'NameMod = T.pack $ show refNameMod
    , ref'NameUnit = T.pack $ show refNameUnit
    , ref'SLine = refSLine
    , ref'SCol = refSCol
    , ref'ELine = refELine
    , ref'ECol = refECol
    }

convertDeclRow :: DeclRow -> DeclRow'
convertDeclRow DeclRow {..} =
  DeclRow'
    { decl'Src = declSrc
    , decl'NameOcc = T.pack $ occNameString declNameOcc
    , decl'SLine = declSLine
    , decl'SCol = declSCol
    , decl'ELine = declELine
    , decl'ECol = declECol
    , decl'Root = declRoot
    }

convertDefRow :: DefRow -> DefRow'
convertDefRow DefRow {..} =
  DefRow'
    { def'Src = defSrc
    , def'NameOcc = T.pack $ occNameString defNameOcc
    , def'SLine = defSLine
    , def'SCol = defSCol
    , def'ELine = defELine
    , def'ECol = defECol
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
      let HieState hieModMap = serverHieState ss
          modHie =
            ModuleHieInfo
              { modHieRefs = fmap convertRefRow refs
              , modHieDecls = fmap convertDeclRow decls
              , modHieDefs = fmap convertDefRow defs
              }
          hieModMap' = M.insert modName modHie hieModMap
       in ss {serverHieState = HieState hieModMap'}
