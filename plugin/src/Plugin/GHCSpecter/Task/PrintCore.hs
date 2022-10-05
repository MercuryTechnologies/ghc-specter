module Plugin.GHCSpecter.Task.PrintCore
  ( printCore,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (packBytes)
import Data.Data (Data (..), dataTypeName, isNorepType, mkNoRepType)
import Data.Foldable (for_)
import Data.Functor.Const (Const (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..), drawForest)
import GHC.Core.Opt.Monad (CoreM, getDynFlags)
import GHC.Unit.Module.ModGuts (ModGuts (..))
import GHCSpecter.Util.GHC (printPpr, showPpr)

gshow :: forall a. Data a => a -> Const [Tree String] a
gshow = gfoldl k mkEmpty
  where
    mkEmpty _ = Const []
    k (Const acc) x =
      let dtyp = dataTypeOf x
          trs = [Node (dataTypeName dtyp) (getConst (gshow x))]
          --  | isNorepType dtyp = [Node (dataTypeName dtyp) []]
          --   | otherwise = [Node (dataTypeName dtyp) (getConst (gshow x))]
          acc' = acc ++ trs
       in Const acc'

printCore :: ModGuts -> CoreM Text
printCore guts = do
  dflags <- getDynFlags
  let binds = mg_binds guts
      txt = T.pack $ showPpr dflags binds
  case binds of
    [] -> pure ()
    (b : _) -> liftIO $ do
      -- for_ binds $ \b -> do
      printPpr dflags b
      putStrLn "========="
      putStrLn $ drawForest (getConst (gshow b))
      threadDelay 10_000_000
  pure txt
