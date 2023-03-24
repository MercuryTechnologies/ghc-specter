{-# LANGUAGE LambdaCase #-}

module GHCSpecter.Util.GHC (
  -- * pretty print
  showPpr,
  printPpr,

  -- * module name
  GHC.ModuleName,
  moduleNameString,
  getModuleName,
  mkModuleNameMap,
  formatName,
  formatImportedNames,

  -- * module graph
  getTopSortedModules,
  extractModuleSources,
  extractModuleGraphInfo,

  -- * Core compat

  -- TODO: These should be moved into a Compat module.
  coreTypeBind,
  coreTypeLiteral,
  coreTypeAltCon,
  coreTypeAlt,
  coreTypeExpr,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (isAlpha)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
import GHC.Data.Graph.Directed qualified as G
import GHC.Driver.Make (moduleGraphNodes, topSortModuleGraph)
import GHC.Driver.Session (DynFlags)
import GHC.Plugins (
  ModSummary,
  Name,
  localiseName,
  showSDoc,
 )
import GHC.Types.Name.Reader (
  GlobalRdrElt (..),
  GreName (..),
  ImpDeclSpec (..),
  ImportSpec (..),
 )
import GHC.Types.SourceFile (HscSource (..))
import GHC.Unit.Module.Graph (
  ModuleGraph,
  ModuleGraphNode (..),
  mgModSummaries,
  mgModSummaries',
 )
import GHC.Unit.Module.Location (ModLocation (..))
import GHC.Unit.Module.ModSummary (ExtendedModSummary (..), ModSummary (..))
import GHC.Unit.Module.Name (moduleNameString)
import GHC.Unit.Module.Name qualified as GHC (ModuleName)
import GHC.Unit.Types (GenModule (moduleName))
import GHC.Utils.Outputable (Outputable (ppr))
import GHCSpecter.Channel.Common.Types (type ModuleName)
import GHCSpecter.Channel.Outbound.Types (ModuleGraphInfo (..))
import System.Directory (canonicalizePath)

--
-- pretty print
--

showPpr :: (Outputable a) => DynFlags -> a -> String
showPpr dflags = showSDoc dflags . ppr

printPpr :: (Outputable a, MonadIO m) => DynFlags -> a -> m ()
printPpr dflags = liftIO . putStrLn . showPpr dflags

--
-- module name
--

-- | Extract module name from ModSummary.
-- For hs-boot and hsig, we rely on stringy suffix ".hs-boot" and ".hsig".

-- TODO: Use HscSource directly (i.e. (ModuleName, HscSource)) to index a module.
getModuleName :: ModSummary -> ModuleName
getModuleName s =
  let sig = ms_hsc_src s
      name = T.pack . moduleNameString . moduleName . ms_mod $ s
   in case sig of
        HsSrcFile -> name
        HsBootFile -> name <> ".hs-boot"
        HsigFile -> name <> ".hsig"

formatName :: DynFlags -> Name -> String
formatName dflags name =
  let str = showSDoc dflags . ppr . localiseName $ name
   in case str of
        (x : _) ->
          -- NOTE: As we want to have resultant text directly copied and pasted to
          --       the source code, the operator identifiers should be wrapped with
          --       parentheses.
          if isAlpha x
            then str
            else "(" ++ str ++ ")"
        _ -> str

formatImportedNames :: [String] -> String
formatImportedNames names =
  case fmap (++ ",\n") $ L.sort names of
    l0 : ls ->
      let l0' = "  ( " ++ l0
          ls' = fmap ("    " ++) ls
          footer = "  )"
       in concat ([l0'] ++ ls' ++ [footer])
    _ -> "  ()"

mkModuleNameMap :: GlobalRdrElt -> [(ModuleName, Name)]
mkModuleNameMap gre = do
  spec <- gre_imp gre
  case gre_name gre of
    NormalGreName name -> do
      let modName = T.pack . moduleNameString . is_mod . is_decl $ spec
      pure (modName, name)
    -- TODO: Handle the record field name case correctly.
    FieldGreName _ -> []

--
-- module graph
--

gnode2ModSummary :: ModuleGraphNode -> Maybe ModSummary
gnode2ModSummary InstantiationNode {} = Nothing
gnode2ModSummary (ModuleNode emod) = Just (emsModSummary emod)

getTopSortedModules :: ModuleGraph -> [ModuleName]
getTopSortedModules modGraph =
  let sccs' = topSortModuleGraph False modGraph Nothing
      allMods = concatMap G.flattenSCC sccs'
      maybeModNameFromModSummary = fmap getModuleName . gnode2ModSummary
      allModNames = mapMaybe maybeModNameFromModSummary allMods
   in allModNames

extractModuleSources :: ModuleGraph -> IO (Map ModuleName FilePath)
extractModuleSources modGraph = do
  M.fromList . catMaybes <$> traverse extract (mgModSummaries modGraph)
  where
    extract ms = do
      let msrcFile = ml_hs_file (ms_location ms)
      msrcFile' <- traverse canonicalizePath msrcFile
      pure $ fmap (getModuleName ms,) msrcFile'

extractModuleGraphInfo :: ModuleGraph -> ModuleGraphInfo
extractModuleGraphInfo modGraph = do
  let (graph, _) = moduleGraphNodes False (mgModSummaries' modGraph)
      vtxs = G.verticesG graph
      modNameFromVertex =
        fmap getModuleName . gnode2ModSummary . G.node_payload
      modNameMapLst =
        mapMaybe
          (\v -> (G.node_key v,) <$> modNameFromVertex v)
          vtxs
      modNameMap :: IntMap ModuleName
      modNameMap = IM.fromList modNameMapLst
      modNameRevMap :: Map ModuleName Int
      modNameRevMap = M.fromList $ fmap swap modNameMapLst
      topSorted =
        mapMaybe
          (\n -> M.lookup n modNameRevMap)
          $ getTopSortedModules modGraph
      modDeps = IM.fromList $ fmap (\v -> (G.node_key v, G.node_dependencies v)) vtxs
   in ModuleGraphInfo modNameMap modDeps topSorted

coreTypeBind :: Text
coreTypeBind = "Bind"

coreTypeLiteral :: Text
coreTypeLiteral = "Literal"

coreTypeAltCon :: Text
coreTypeAltCon = "AltCon"

coreTypeAlt :: Text
coreTypeAlt = "Alt"

coreTypeExpr :: Text
coreTypeExpr = "Expr"
