module Plugin.GHCSpecter.Task.PrintCore
  ( printCore,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Internal (packBytes)
import Data.Data (Data (..), cast, dataTypeName, isNorepType, mkNoRepType)
import Data.Foldable (for_)
import Data.Functor.Const (Const (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Tree (Tree (..), drawForest)
import GHC.Core.Opt.Monad (CoreM, getDynFlags)
import GHC.Core.TyCon (TyCon)
import GHC.Types.Name (NamedThing (getOccName))
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Var (Var)
import GHC.Unit.Module.ModGuts (ModGuts (..))
import GHCSpecter.Util.GHC (printPpr, showPpr)

core2tree :: forall a. Data a => a -> Const [Tree (Text, Text)] a
core2tree = gfoldl k mkEmpty
  where
    mkEmpty _ = Const []
    k (Const acc) x =
      let dtyp = dataTypeOf x
          dtypName = dataTypeName dtyp
          trs
            | dtypName == "Data.ByteString.ByteString" =
                let mbs = cast x
                    content = maybe "#######" decodeUtf8 mbs
                    info = (T.pack dtypName, content)
                 in [Node info []]
            | dtypName == "Var" =
                let mvar :: Maybe Var
                    mvar = cast x
                    content = maybe "#######" (T.pack . occNameString . getOccName) mvar
                    info = (T.pack dtypName, content)
                 in [Node info []]
            | dtypName == "TyCon" =
                let mtyc :: Maybe TyCon
                    mtyc = cast x
                    content = maybe "#######" (T.pack . occNameString . getOccName) mtyc
                    info = (T.pack dtypName, content)
                 in [Node info []]
            | otherwise =
                let info = (T.pack dtypName, T.pack (show (toConstr x)))
                 in [Node info (getConst (core2tree x))]
       in Const (acc ++ trs)

printCore :: ModGuts -> CoreM Text
printCore guts = do
  dflags <- getDynFlags
  let binds = mg_binds guts
      txt = T.pack $ showPpr dflags binds
  liftIO $ do
    for_ binds $ \b -> do
      printPpr dflags b
      putStrLn "========="
      putStrLn $ drawForest (fmap (fmap show) $ getConst (core2tree b))
  pure txt
