{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | CheckImport plugin:
--   This plugin checks if imported identifiers as unqualified
--   exist and lists them.
module Plugin.CheckImports
  ( -- NOTE: The name "plugin" should be used to be called via GHC plugin mechanism.
    plugin,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (isAlpha)
import Data.IORef (readIORef)
import Data.List (foldl', sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Driver.Session (DynFlags, getDynFlags)
import GHC.Plugins
  ( CommandLineOption,
    ModSummary,
    Name,
    Plugin (..),
    defaultPlugin,
    localiseName,
    showSDoc,
  )
import GHC.Tc.Types (TcGblEnv (..), TcM)
import GHC.Types.Name.Reader
  ( GlobalRdrElt (..),
    GreName (..),
    ImpDeclSpec (..),
    ImportSpec (..),
  )
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHC.Unit.Module.Name (ModuleName, moduleNameString)
import GHC.Unit.Types (GenModule (moduleName))
import GHC.Utils.Outputable (Outputable (ppr))
import Toolbox.Comm (runClient, sendObject)
import Prelude hiding ((<>))

plugin :: Plugin
plugin =
  defaultPlugin
    { typeCheckResultAction = typecheckPlugin
    }

showPpr :: (Outputable a) => DynFlags -> a -> String
showPpr dflags = showSDoc dflags . ppr

printPpr :: (Outputable a, MonadIO m) => DynFlags -> a -> m ()
printPpr dflags = liftIO . putStrLn . showPpr dflags

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
  case fmap (++ ",\n") $ sort names of
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
      let modName = is_mod $ is_decl spec
      pure (modName, name)
    -- TODO: Handle the record field name case correctly.
    FieldGreName _ -> []

typecheckPlugin ::
  [CommandLineOption] ->
  ModSummary ->
  TcGblEnv ->
  TcM TcGblEnv
typecheckPlugin _ modsummary tc = do
  dflags <- getDynFlags
  usedGREs :: [GlobalRdrElt] <-
    liftIO $ readIORef (tcg_used_gres tc)
  let moduleImportMap :: Map ModuleName (Set Name)
      moduleImportMap =
        foldl' (\(!m) (modu, name) -> M.insertWith S.union modu (S.singleton name) m) M.empty $
          concatMap mkModuleNameMap usedGREs

  let rendered =
        unlines $
          flip concatMap (M.toList moduleImportMap) $ \(modu, names) ->
            let imported = fmap (formatName dflags) $ S.toList names
             in [showPpr dflags modu, formatImportedNames imported]
  printPpr dflags modsummary

  let modName = T.pack $ moduleNameString $ moduleName $ ms_mod modsummary
  liftIO $
    runClient "/tmp/ghc-build-analyzer.ipc" $ \sock ->
      sendObject sock (modName, rendered)
  pure tc
