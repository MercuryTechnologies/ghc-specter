{-# LANGUAGE RecordWildCards #-}

module Toolbox.Worker.CallGraph
  ( callGraphWorker,
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
import Text.Pretty.Simple
import Toolbox.Channel (HsSourceInfo (..))
import Toolbox.Server.Types
  ( HieState (..),
    RefRow' (..),
    ServerState (..),
  )
import Toolbox.Util.GHC (printPpr)


-- orphan instances for now

instance Show OccName where
  show = occNameString

-- instance FromJSON OccName where
--   occNameString
  
deriving instance Show RefRow

-- deriving instance Generic RefRow

deriving instance Show DeclRow

-- deriving instance Generic DeclRow

deriving instance Show DefRow

-- deriving instance Generic DefRow

convertRefRow :: RefRow -> RefRow'
convertRefRow RefRow {..} =
  RefRow'
    { ref'Src = refSrc
    , ref'NameOcc = T.pack $ show refNameOcc
    , ref'NameMod = T.pack $ show refNameMod
    , ref'NameUnit = T.pack $ show refNameUnit
    , ref'SLine = refSLine
    , ref'SCol = refSCol
    , ref'ELine = refELine
    , ref'ECol = refECol
    }

callGraphWorker :: TVar ServerState -> FilePath -> IO ()
callGraphWorker var hiefile = do
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
  let hieState = HieState $
        [(modName, fmap convertRefRow refs)]
  atomically $
    modifyTVar' var $ \ss ->
      ss {serverHieState = hieState}

