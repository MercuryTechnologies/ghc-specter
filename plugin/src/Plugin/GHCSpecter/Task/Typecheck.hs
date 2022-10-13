{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Plugin.GHCSpecter.Task.Typecheck
  ( fetchUnqualifiedImports,
    testSourceLocation,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Data.Bag (bagToList)
import GHC.Driver.Session (getDynFlags)
import GHC.Plugins (Name)
import GHC.Tc.Types (TcGblEnv (..), TcM)
import GHC.Types.Name.Reader (GlobalRdrElt (..))
import GHC.Types.SrcLoc (getLoc, unLoc)
import GHCSpecter.Channel.Common.Types
  ( type ModuleName,
  )
import GHCSpecter.Channel.Outbound.Types (ConsoleReply (..))
import GHCSpecter.Util.GHC (printPpr, showPpr)
import Language.Haskell.Syntax.Binds (HsBindLR (..))
import Plugin.GHCSpecter.Util
  ( formatImportedNames,
    formatName,
    mkModuleNameMap,
  )

fetchUnqualifiedImports :: TcGblEnv -> TcM ConsoleReply
fetchUnqualifiedImports tc = do
  dflags <- getDynFlags
  usedGREs :: [GlobalRdrElt] <- liftIO $ readIORef (tcg_used_gres tc)
  let moduleImportMap :: Map ModuleName (Set Name)
      moduleImportMap =
        L.foldl' (\(!m) (modu, name) -> M.insertWith S.union modu (S.singleton name) m) M.empty $
          concatMap mkModuleNameMap usedGREs
      rendered =
        unlines $ do
          (modu, names) <- M.toList moduleImportMap
          let imported = fmap (formatName dflags) $ S.toList names
          [T.unpack modu, formatImportedNames imported]
  pure (ConsoleReplyText (T.pack rendered))

testSourceLocation :: TcGblEnv -> TcM ConsoleReply
testSourceLocation tc = do
  dflags <- getDynFlags
  let extract FunBind {..} = [fun_id] --  Nothing -- "funbind" -- Just (unLoc fun_id)
      extract VarBind {..} = [] -- "varbind" -- Just var_id
      extract PatBind {..} = [] -- "patbind"
      extract AbsBinds {..} =
        concatMap (extract . unLoc) (bagToList abs_binds)
      -- Just () -- "absbinds"
      extract PatSynBind {} = [] -- "patsynbind"
      extract XHsBindsLR {} = [] -- "xhsbindsLR"
      extract _ = []
      format i =
        T.pack $
          showPpr dflags (getLoc i) ++ ":" ++ showPpr dflags (unLoc i)
  let bs = bagToList $ tcg_binds tc
      is = concatMap (extract . unLoc) bs
      txt = T.intercalate "\n" $ fmap format is

  -- txt = T.intercalate " " $ fmap (format . unLoc) bs
  liftIO $ printPpr dflags bs
  pure (ConsoleReplyText txt)

{-  usedGREs :: [GlobalRdrElt] <- liftIO $ readIORef (tcg_used_gres tc)
  let moduleImportMap :: Map ModuleName (Set Name)
      moduleImportMap =
        L.foldl' (\(!m) (modu, name) -> M.insertWith S.union modu (S.singleton name) m) M.empty $
          concatMap mkModuleNameMap usedGREs
      rendered =
        unlines $ do
          (modu, names) <- M.toList moduleImportMap
          let imported = fmap (formatName dflags) $ S.toList names
          [T.unpack modu, formatImportedNames imported]
  pure (ConsoleReplyText (T.pack rendered))
-}
