{-# LANGUAGE RecordWildCards #-}

module Toolbox.Worker.Hie
  ( hieWorker,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Monad (void)
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Tree
import GHC.Driver.Pipeline
import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils
import GHC.Plugins hiding (ModuleName)
import GHC.SysTools
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Types.SrcLoc
import GHC.Types.Unique.Supply
import HieDb.Compat (OccName, nameModule, occNameString)
import HieDb.Types
import HieDb.Utils
import Toolbox.Channel (HsSourceInfo (..))
import Toolbox.Server.Types
  ( DeclRow' (..),
    DefRow' (..),
    HieState (..),
    ModuleHieInfo (..),
    RefRow' (..),
    ServerState (..),
  )
import Toolbox.Util.GHC (printPpr)

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
      mod = hie_module hf
      modName = T.pack $ moduleNameString $ moduleName mod
      asts = hie_asts hf
      refmap = generateReferencesMap $ getAsts asts
      (refs, decls) = genRefsAndDecls "" mod refmap
      defs = genDefRow "" mod refmap
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
