{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module GHCSpecter.Util.GHC (
  -- * pretty print
  showPpr,
  printPpr,

  -- * module name
  getModuleName,
  mkModuleNameMap,
  formatName,
  formatImportedNames,

  -- * module graph
  getTopSortedModules,
  extractModuleSources,
  extractModuleGraphInfo,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (isAlpha)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text qualified as T
import Data.Tuple (swap)
import GHC.Data.Graph.Directed qualified as G
import GHC.Driver.Make (topSortModuleGraph)
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
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHC.Unit.Module.Name (moduleNameString)
import GHC.Unit.Types (GenModule (moduleName))
import GHC.Utils.Outputable (Outputable (ppr))
import GHCSpecter.Channel.Common.Types (type ModuleName)
import GHCSpecter.Channel.Outbound.Types (ModuleGraphInfo (..))
import System.Directory (canonicalizePath)
-- GHC-version-dependent imports
#if MIN_VERSION_ghc(9, 4, 0)
import GHC.Data.Bag (bagToList)
import GHC.Unit.Module.Graph (moduleGraphNodes)
#elif MIN_VERSION_ghc(9, 2, 0)
import GHC.Driver.Make (moduleGraphNodes)
import GHC.Unit.Module.ModSummary (ExtendedModSummary (..))
#endif


import Debug.Trace

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

getModuleName :: ModSummary -> ModuleName
getModuleName s =
  let mod = ms_mod s
      sig = ms_hsc_src s
      name = T.pack . moduleNameString . moduleName $ mod
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

--
-- module graph
--

gnode2ModSummary :: ModuleGraphNode -> Maybe ModSummary
gnode2ModSummary InstantiationNode {} = Nothing
#if MIN_VERSION_ghc(9, 4, 0)
gnode2ModSummary (ModuleNode _ modSummary) = Just modSummary
gnode2ModSummary LinkNode {} = Nothing
#else
gnode2ModSummary (ModuleNode emod) = Just (emsModSummary emod)
#endif

-- temporary function. ignore hs-boot cycles and InstantiatedUnit for now.
getTopSortedModules :: ModuleGraph -> ([ModuleName], [[ModuleName]])
getTopSortedModules modGraph =
  let sccs' = topSortModuleGraph False modGraph Nothing
      sccs = topSortModuleGraph True modGraph Nothing
      aMods = mapMaybe (\case G.AcyclicSCC v -> Just v; _ -> Nothing) sccs
      cMods = mapMaybe (\case G.CyclicSCC vs -> Just vs; _ -> Nothing) sccs
      allMods = concatMap G.flattenSCC sccs'
      maybeModNameFromModSummary = fmap getModuleName . gnode2ModSummary
      allModNames = mapMaybe maybeModNameFromModSummary allMods
      cyclicModNames = fmap (mapMaybe maybeModNameFromModSummary) cMods
   in trace (show (length aMods, length cMods, length allMods)) $ (allModNames, cyclicModNames)
    -- . flip (topSortModuleGraph False) Nothing

extractModuleSources :: ModuleGraph -> IO (Map ModuleName FilePath)
extractModuleSources modGraph = do
  M.fromList . catMaybes <$> traverse extract (mgModSummaries modGraph)
  where
    extract ms = do
      let msrcFile = ml_hs_file (ms_location ms)
      msrcFile' <- traverse canonicalizePath msrcFile
      pure $ fmap (getModuleName ms,) msrcFile'

extractModuleGraphInfo :: ModuleGraph -> (ModuleGraphInfo, [ModuleName], [[ModuleName]])
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
          $ fst (getTopSortedModules modGraph)
      modDeps = IM.fromList $ fmap (\v -> (G.node_key v, G.node_dependencies v)) vtxs
      (allMods, nonTrivialSccs) =
        let topMods = getTopSortedModules modGraph
            cyclicModNames = snd topMods
         in (fst topMods, cyclicModNames)

   in (ModuleGraphInfo modNameMap modDeps topSorted, allMods, nonTrivialSccs)
