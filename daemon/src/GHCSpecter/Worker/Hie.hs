{-# LANGUAGE RecordWildCards #-}

module GHCSpecter.Worker.Hie
  ( hieWorker,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Lens ((%~), (.~))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import GHC.Iface.Ext.Binary
  ( HieFileResult (..),
    NameCacheUpdater (NCU),
    readHieFile,
  )
import GHC.Iface.Ext.Types (HieFile (..), getAsts)
import GHC.Iface.Ext.Utils (generateReferencesMap)
import GHC.Types.Name.Cache (initNameCache)
import GHCSpecter.Server.Types
  ( DeclRow' (..),
    DefRow' (..),
    HasHieState (..),
    HasModuleHieInfo (..),
    HasServerState (..),
    RefRow' (..),
    ServerState (..),
    emptyModuleHieInfo,
  )
import HieDb.Compat
  ( mkSplitUniqSupply,
    moduleName,
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

hieWorker :: TVar ServerState -> FilePath -> IO ()
hieWorker var hiefile = do
  uniq_supply <- mkSplitUniqSupply 'z'
  let nc = initNameCache uniq_supply []
  hieResult <- readHieFile (NCU (\f -> pure $ snd $ f nc)) hiefile
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

  atomically $
    modifyTVar' var $
      serverHieState . hieModuleMap
        %~ M.insert modName modHie
