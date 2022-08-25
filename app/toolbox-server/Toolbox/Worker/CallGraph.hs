module Toolbox.Worker.CallGraph
  ( callGraphWorker,
  )
where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe (fromJust)
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
import Toolbox.Server.Types (ServerState)
import Toolbox.Util.GHC (printPpr)

instance Show OccName where
  show = occNameString

deriving instance Show RefRow

deriving instance Show DeclRow

deriving instance Show DefRow

callGraphWorker :: FilePath -> IO ()
callGraphWorker hiefile = void $
  forkIO $ do
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
    print refsDecls
    putStrLn "-- Defs --"
    print defs
