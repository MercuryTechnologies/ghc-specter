module Toolbox.Worker.CallGraph
  ( tempWorker
  )
where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Toolbox.Server.Types (ServerState)


import GHC.Types.Name.Cache
import GHC.Types.SrcLoc
import GHC.Types.Unique.Supply
import GHC.Types.Name
import Data.Tree
import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils
import Data.Maybe (fromJust)
import GHC.Driver.Pipeline
import GHC.SysTools
import qualified Data.Map as M
import Data.Foldable
import Toolbox.Channel (HsSourceInfo (..))
import Toolbox.Util.GHC (printPpr)
import GHC.Plugins hiding (ModuleName)
import HieDb.Compat (OccName, nameModule, occNameString)
import HieDb.Types
import HieDb.Utils
import Text.Pretty.Simple

instance Show OccName where
  show = occNameString

deriving instance Show RefRow

deriving instance Show DeclRow

deriving instance Show DefRow

tempWorker :: FilePath -> IO ()
tempWorker hiefile = void $ forkIO $ do
  uniq_supply <- mkSplitUniqSupply 'z'
  let nc = initNameCache uniq_supply []
  hieResult <- readHieFile (NCU (\f -> pure $ snd $ f nc)) hiefile
  let hf = hie_file_result hieResult
      mod = hie_module hf
      asts = hie_asts hf
      refmap = generateReferencesMap $ getAsts asts
  -- printPpr dflags asts
  let -- mod = nameModule "A"
      refsDecls = genRefsAndDecls "" mod refmap
      defs = genDefRow "" mod refmap
  -- printPpr dflags refmap
  putStrLn "-- Refs and Decls --"
  pPrint refsDecls
  putStrLn "-- Defs --"
  pPrint defs
