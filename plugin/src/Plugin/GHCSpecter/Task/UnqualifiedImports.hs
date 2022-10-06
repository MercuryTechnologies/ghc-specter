module Plugin.GHCSpecter.Task.UnqualifiedImports
  ( fetchUnqualifiedImports,
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
import GHC.Driver.Session (getDynFlags)
import GHC.Plugins (Name)
import GHC.Tc.Types (TcGblEnv (..), TcM)
import GHC.Types.Name.Reader (GlobalRdrElt (..))
import GHCSpecter.Channel.Common.Types
  ( type ModuleName,
  )
import GHCSpecter.Channel.Outbound.Types (ConsoleReply (..))
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
