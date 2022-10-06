module Plugin.GHCSpecter.Task.PrintCore
  ( printCore,
  )
where

import Data.Data (Data (..), cast, dataTypeName)
import Data.Functor.Const (Const (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Tree (Tree (..))
import GHC.Core.Opt.Monad (CoreM, getDynFlags)
import GHC.Core.TyCon (TyCon)
import GHC.Types.Name (NamedThing (getOccName))
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Var (Var)
import GHC.Unit.Module.ModGuts (ModGuts (..))
import GHCSpecter.Channel.Outbound.Types (ConsoleReply (..))

import Control.Monad.IO.Class
import Data.Tree

-- | Left: ordinary node, Right: short-circuit to terminal
getContent :: forall a. Data a => a -> (Text, Either Text (Maybe Text))
getContent x = (T.pack dtypName, evalue)
  where
    dtyp = dataTypeOf x
    dtypName = dataTypeName dtyp
    evalue
      | dtypName == "Data.ByteString.ByteString" =
          let mbs = cast x
           in Right (fmap decodeUtf8 mbs)
      | dtypName == "Var" =
          let mvar :: Maybe Var
              mvar = cast x
           in Right (fmap (T.pack . occNameString . getOccName) mvar)
      | dtypName == "TyCon" =
          let mtyc :: Maybe TyCon
              mtyc = cast x
           in Right (fmap (T.pack . occNameString . getOccName) mtyc)
      | otherwise = Left (T.pack (show (toConstr x)))

core2tree :: forall a. Data a => a -> Const [Tree (Text, Text)] a
core2tree = gfoldl k mkEmpty
  where
    mkEmpty _ = Const []
    k (Const acc) x =
      let delta =
            case getContent x of
              (typ, Left val) -> [Node (typ, val) (getConst (core2tree x))]
              (typ, Right (Just val)) -> [Node (typ, val) []]
              (typ, Right Nothing) -> [Node (typ, "#######") []]
       in Const (acc ++ delta)

printCore :: ModGuts -> CoreM ConsoleReply
printCore guts = do
  let binds = mg_binds guts
      forest = getConst (core2tree binds)
  -- liftIO $ putStrLn $ drawForest . fmap (fmap show) $ forest
  pure (ConsoleReplyCore forest)
