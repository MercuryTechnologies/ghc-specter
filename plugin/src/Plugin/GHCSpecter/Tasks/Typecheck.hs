module Plugin.GHCSpecter.Tasks.Typecheck (
  fetchUnqualifiedImports,
  showRenamed,
  showRnSplice,
  showSpliceExpr,
  showSpliceResult,
) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Driver.Session (DynFlags, getDynFlags)
import GHC.Hs.Extension (GhcRn, GhcTc)
import GHC.Plugins (Name)
import GHC.Tc.Types (RnM, TcGblEnv (..), TcM)
import GHC.Types.Name.Reader (GlobalRdrElt (..))
import GHC.Utils.Outputable (Outputable)
import GHCSpecter.Channel.Common.Types (
  type ModuleName,
 )
import GHCSpecter.Channel.Outbound.Types (ConsoleReply (..))
import GHCSpecter.Util.GHC (showPpr)
import Language.Haskell.Syntax.Decls (HsGroup)
import Language.Haskell.Syntax.Expr (HsSplice, LHsExpr)
import Plugin.GHCSpecter.Util (
  formatImportedNames,
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
  pure (ConsoleReplyText Nothing (T.pack rendered))

showRenamed :: HsGroup GhcRn -> TcM ConsoleReply
showRenamed grp = do
  dflags <- getDynFlags
  let txt = T.pack (showPpr dflags grp)
  pure (ConsoleReplyText (Just "renamed") txt)

showRnSplice :: HsSplice GhcRn -> RnM ConsoleReply
showRnSplice splice = do
  dflags <- getDynFlags
  let txt = T.pack (showPpr dflags splice)
  pure (ConsoleReplyText (Just "splice") txt)

showSpliceExpr :: LHsExpr GhcTc -> TcM ConsoleReply
showSpliceExpr expr = do
  dflags <- getDynFlags
  let txt = T.pack (showPpr dflags expr)
  pure (ConsoleReplyText (Just "splice-expr") txt)

showSpliceResult :: Outputable r => DynFlags -> r -> ConsoleReply
showSpliceResult dflags result =
  let txt = T.pack (showPpr dflags result)
   in ConsoleReplyText (Just "meta") txt
