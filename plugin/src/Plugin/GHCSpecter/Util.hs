{-# LANGUAGE CPP #-}

module Plugin.GHCSpecter.Util
  ( -- * Utilities
    getTopSortedModules,
    extractModuleSources,
    extractModuleGraphInfo,
    getModuleName,
    mkModuleNameMap,
    formatName,
    formatImportedNames,
  )
where

import Data.Char (isAlpha)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text qualified as T
import Data.Tuple (swap)
import GHC.Data.Bag (bagToList)
import GHC.Data.Graph.Directed qualified as G
import GHC.Driver.Make (topSortModuleGraph)
#if MIN_VERSION_ghc(9, 4, 0)
import GHC.Unit.Module.Graph (moduleGraphNodes)
#elif MIN_VERSION_ghc(9, 2, 0)
import GHC.Driver.Make (moduleGraphNodes)
#endif
import GHC.Driver.Session (DynFlags)
import GHC.Plugins
  ( ModSummary,
    Name,
    localiseName,
    showSDoc,
  )
import GHC.Types.Name.Reader
  ( GlobalRdrElt (..),
    GreName (..),
    ImpDeclSpec (..),
    ImportSpec (..),
  )
import GHC.Unit.Module.Graph
  ( ModuleGraph,
    ModuleGraphNode (..),
    mgModSummaries,
    mgModSummaries',
  )
import GHC.Unit.Module.Location (ModLocation (..))
import GHC.Unit.Module.ModIface (ModIface_ (mi_module))
#if MIN_VERSION_ghc(9, 4, 0)
#elif MIN_VERSION_ghc(9, 2, 0)
import GHC.Unit.Module.ModSummary (ExtendedModSummary (..))
#endif
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHC.Unit.Module.Name (moduleNameString)
import GHC.Unit.Types (GenModule (moduleName))
import GHC.Utils.Outputable (Outputable (ppr))
import GHCSpecter.Channel.Common.Types (type ModuleName)
import GHCSpecter.Channel.Outbound.Types (ModuleGraphInfo (..))
import System.Directory (canonicalizePath)

getModuleName :: ModSummary -> ModuleName
getModuleName = T.pack . moduleNameString . moduleName . ms_mod

gnode2ModSummary :: ModuleGraphNode -> Maybe ModSummary
gnode2ModSummary InstantiationNode {} = Nothing
#if MIN_VERSION_ghc (9, 4, 0)
gnode2ModSummary (ModuleNode _ modSummary) = Just modSummary
#elif MIN_VERSION_ghc (9, 2, 0)
gnode2ModSummary (ModuleNode emod) = Just (emsModSummary emod)
#endif
gnode2ModSummary LinkNode {} = Nothing

-- temporary function. ignore hs-boot cycles and InstantiatedUnit for now.
getTopSortedModules :: ModuleGraph -> [ModuleName]
getTopSortedModules =
  mapMaybe (fmap getModuleName . gnode2ModSummary)
    . concatMap G.flattenSCC
    . flip (topSortModuleGraph False) Nothing

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
#if MIN_VERSION_ghc(9, 4, 0)
  spec <- bagToList (gre_imp gre)
#elif MIN_VERSION_ghc(9, 2, 0)
  spec <- gre_imp gre
#endif
  case gre_name gre of
    NormalGreName name -> do
      let modName = T.pack . moduleNameString . is_mod . is_decl $ spec
      pure (modName, name)
    -- TODO: Handle the record field name case correctly.
    FieldGreName _ -> []
