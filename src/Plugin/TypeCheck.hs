{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

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
import Data.Data
import Data.Dynamic
import Data.Functor.Const
import Data.IORef (readIORef)
import Data.List (foldl', sort)
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Monoid as M ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Data.Bag (bagToList)
import GHC.Data.IOEnv (getEnv)
import GHC.Driver.Env.Types (Hsc)
import GHC.Driver.Session (DynFlags, getDynFlags)
import GHC.Hs (HsParsedModule)
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
import GHC.Tc.Types
  ( Env (..),
    TcGblEnv (..),
    TcLclEnv (..),
    TcM,
  )
import GHC.Types.Avail (Avails)
import GHC.Types.Name.Reader
  ( GlobalRdrElt (..),
    GreName (..),
    ImpDeclSpec (..),
    ImportSpec (..),
    Parent,
    greNameSrcSpan,
  )
import GHC.Types.SrcLoc (SrcSpan, unLoc)
import GHC.Types.Var (Id)
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHC.Unit.Module.Name (ModuleName, moduleNameString)
import GHC.Unit.Types (GenModule (moduleName))
import GHC.Utils.Outputable (Outputable (ppr))
import Language.Haskell.Syntax.Binds
  ( HsBindLR (..),
    LHsBinds,
  )
import Language.Haskell.Syntax.Decls (HsGroup)
import Language.Haskell.Syntax.Expr
  ( HsExpr (..),
    LHsExpr,
    MatchGroup (..),
  )
import PyF (fmt)
import System.Directory (doesFileExist)
import Toolbox.Channel
  ( ChanMessage (CMTypeCheck),
    ChanMessageBox (..),
  )
import Toolbox.Comm (runClient, sendObject)
import Toolbox.Util (printPpr, printPprDebug, showPpr, showPprDebug)
import Prelude hiding ((<>))

plugin :: Plugin
plugin =
  defaultPlugin
    { parsedResultAction = parsedResultPlugin
    , typeCheckResultAction = typecheckPlugin
    }


parsedResultPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedResultPlugin _ modSummary hpm = do
  -- mkHieFile modSummary
  pure hpm

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

formatId :: DynFlags -> Id -> Text
formatId dflags i =
  T.pack (showPpr dflags i)
  -- [fmt|{showPpr dflags i}|]  -- : {showPprDebug dflags i})|]

{-
getFunctionDeps :: HsBindLR GhcTc GhcTc -> [(Id, [Id])]
getFunctionDeps binding =
    case binding of
      -- function/variable binding
      FunBind {fun_id, fun_matches} ->
        [(unLoc fun_id, depsFromFunBind fun_matches)]
      PatBind {} -> []
      -- dictionary binding
      VarBind {var_id} -> [(var_id, [])]
      AbsBinds {abs_binds} -> concatMap (getFunctionDeps . unLoc) (bagToList abs_binds)
      PatSynBind {} -> []
      XHsBindsLR {} -> []
  where
    depsFromFunBind :: MatchGroup GhcTc (LHsExpr GhcTc) -> [Id]
    depsFromFunBind MG {mg_alts} = concatMap go mg_alts
      where
        go Match {grhss}
    depsFromFunBind (XMatchGroup _) = []
-}

extractIds :: HsBindLR GhcTc GhcTc -> Int -- [Id]
extractIds binding =
    getConst (go binding)
  where
    go :: Data x => x -> Const {- [Id] -} Int x
    go = gfoldl k (\_ -> Const 0)
    k ::
      forall d b. Data d => Const Int {- [Id] -} (d -> b) -> d -> Const Int {- [Id] -} b
    Const (!ys) `k` x =
      let zs =
            if typeOf x == typeOf (undefined :: Id)
              then 1 -- maybeToList . fromDynamic . toDyn $ x
              else
                let Const zs' = go x
                 in zs'
       in Const {- (ys ++ zs) -} (ys + zs)

getAllGreNameSrcSpans :: [GlobalRdrElt] -> [(GreName, SrcSpan)]
getAllGreNameSrcSpans gres = do
  gre <- gres
  let grename = gre_name gre
  pure (grename, greNameSrcSpan grename)

{-
formatGreNameSrcSpan :: DynFlags -> (GreName, SrcSpan) -> Text
formatGreNameSrcSpan dflags (grename, srcspan) =
  [fmt|{showPpr dflags grename}: {showPprDebug dflags grename}, {showPpr dflags srcspan}: {showPprDebug dflags srcspan}|]
-}

-- | First argument in -fplugin-opt is interpreted as the socket file path.
--   If nothing, do not try to communicate with web frontend.
typecheckPlugin ::
  [CommandLineOption] ->
  ModSummary ->
  TcGblEnv ->
  TcM TcGblEnv
typecheckPlugin opts modsummary tc = do
  tlEnv <- env_lcl <$> getEnv
  dflags <- getDynFlags
  -- printPprDebug dflags (tcl_env tlEnv)
  -- printPprDebug dflags (tcg_rdr_env tc)
  usedGREs :: [GlobalRdrElt] <-
    liftIO $ readIORef (tcg_used_gres tc)
  let modNameMap = concatMap mkModuleNameMap usedGREs

      moduleImportMap :: Map ModuleName (Set Name)
      moduleImportMap =
        foldl' (\(!m) (modu, name) -> M.insertWith S.union modu (S.singleton name) m) M.empty $
          modNameMap

  let tc_binds = bagToList $ tcg_binds tc
      tcs = tcg_tcs tc
{-  
  liftIO $
    TIO.putStrLn $
      T.intercalate "\n------\n" $
        flip fmap tc_binds $ \(unLoc -> binding) ->
          {- T.pack (showPpr dflags binding)
            M.<> "\n"
            M.<> -} {- T.intercalate
                   ", " -}
                   ({- fmap (formatId dflags) -} (T.pack . show) (extractIds binding))
-}
--        (formatId dflags) $
--          concatMap (extractIds . unLoc) tc_binds

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
