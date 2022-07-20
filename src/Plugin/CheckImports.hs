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
import qualified Data.ByteString.Char8 as C
import Data.Char (isAlpha)
import Data.IORef (readIORef)
import Data.List (foldl', sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
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
import GHC.Unit.Module (ModuleName)
import GHC.Utils.Outputable (Outputable (ppr))
import Network.Socket.ByteString (sendAll)
import Toolbox.Comm (runClient)
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
    _ -> []

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
             in ["---------", showPpr dflags modu, formatImportedNames imported]
  printPpr dflags modsummary

  liftIO $
    runClient "/tmp/ghc-build-analyzer.ipc" $ \s -> sendAll s (C.pack rendered)
  pure tc
