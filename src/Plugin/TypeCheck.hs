{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | TypeCheck plugin:
--   This plugin runs at type checker time.
module Plugin.TypeCheck
  ( -- NOTE: The name "plugin" should be used as a GHC plugin.
    plugin,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (isAlpha)
import Data.IORef (readIORef)
import Data.List (foldl', sort)
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Data.Bag (bagToList)
import GHC.Driver.Session (DynFlags, getDynFlags)
import GHC.Hs.Doc (LHsDocString)
import GHC.Hs.Extension (GhcRn, GhcTc)
import GHC.Hs.ImpExp (LIE, LImportDecl)
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
import GHC.Types.Avail (Avails)
import GHC.Types.Name.Reader
  ( GlobalRdrElt (..),
    GreName (..),
    ImpDeclSpec (..),
    ImportSpec (..),
  )
import GHC.Types.SrcLoc (unLoc)
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHC.Unit.Module.Name (ModuleName, moduleNameString)
import GHC.Unit.Types (GenModule (moduleName))
import GHC.Utils.Outputable (Outputable (ppr))
import Language.Haskell.Syntax.Binds
  ( HsBindLR (..),
    LHsBinds,
  )
import Language.Haskell.Syntax.Decls (HsGroup)
import System.Directory (doesFileExist)
import Toolbox.Channel
  ( ChanMessage (CMTypeCheck),
    ChanMessageBox (..),
  )
import Toolbox.Comm (runClient, sendObject)
import Toolbox.Util (printPpr, showPpr)
import Prelude hiding ((<>))

plugin :: Plugin
plugin =
  defaultPlugin
    { typeCheckResultAction = typecheckPlugin
    }

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

type RenamedSource =
  (HsGroup GhcRn, [LImportDecl GhcRn], Maybe [(LIE GhcRn, Avails)], Maybe LHsDocString)

type TypecheckedSource = LHsBinds GhcTc

-- | First argument in -fplugin-opt is interpreted as the socket file path.
--   If nothing, do not try to communicate with web frontend.
typecheckPlugin ::
  [CommandLineOption] ->
  ModSummary ->
  TcGblEnv ->
  TcM TcGblEnv
typecheckPlugin opts modsummary tc = do
  dflags <- getDynFlags
  usedGREs :: [GlobalRdrElt] <-
    liftIO $ readIORef (tcg_used_gres tc)
  let moduleImportMap :: Map ModuleName (Set Name)
      moduleImportMap =
        foldl' (\(!m) (modu, name) -> M.insertWith S.union modu (S.singleton name) m) M.empty $
          concatMap mkModuleNameMap usedGREs

  let tc_binds = bagToList $ tcg_binds tc
      tcs = tcg_tcs tc

      showBind =
        \case
          FunBind {fun_id} -> "FunBind: " ++ showPpr dflags fun_id
          PatBind {} -> "PatBind"
          VarBind {var_id} -> "VarBind: " ++ showPpr dflags var_id
          AbsBinds {abs_binds} ->
            "AbsBinds: " ++ L.intercalate ", " (fmap (showBind . unLoc) $ bagToList abs_binds)
          PatSynBind {} -> "PatSynBind"
          XHsBindsLR {} -> "XHsBindsLR"
      test = fmap (showBind . unLoc) tc_binds

  printPpr dflags tc_binds
  printPpr dflags tcs
  liftIO $ print test

  let rendered =
        unlines $ do
          (modu, names) <- M.toList moduleImportMap
          let imported = fmap (formatName dflags) $ S.toList names
          [showPpr dflags modu, formatImportedNames imported]
  -- printPpr dflags modsummary

  let modName = T.pack $ moduleNameString $ moduleName $ ms_mod modsummary
  case opts of
    ipcfile : _ -> liftIO $ do
      socketExists <- doesFileExist ipcfile
      when socketExists $
        runClient ipcfile $ \sock ->
          sendObject sock (CMBox (CMTypeCheck modName (T.pack rendered)))
    _ -> pure ()
  pure tc
