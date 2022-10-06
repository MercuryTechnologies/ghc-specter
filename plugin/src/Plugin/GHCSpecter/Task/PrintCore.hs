module Plugin.GHCSpecter.Task.PrintCore
  ( printCore,
  )
where

import Data.ByteString.Short qualified as SB
import Data.Data (Data (..), cast, dataTypeName)
import Data.Functor.Const (Const (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Tree (Tree (..))
import Data.Typeable (Typeable)
import GHC.Core.Class (Class)
import GHC.Core.ConLike (ConLike)
import GHC.Core.DataCon (DataCon)
import GHC.Core.Opt.Monad (CoreM)
import GHC.Core.PatSyn (PatSyn)
import GHC.Core.TyCon (TyCon)
import GHC.Data.FastString (FastString (..))
import GHC.Types.Name (Name, NamedThing (getOccName), OccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc (RealSrcSpan, SrcSpan)
import GHC.Types.Var (Var)
import GHC.Unit.Module.ModGuts (ModGuts (..))
import GHC.Unit.Module.Name (ModuleName, moduleNameString)
import GHC.Unit.Types (Unit, toUnitId, unitString)
import GHCSpecter.Channel.Outbound.Types (ConsoleReply (..))

{-
data SemiExpr = Bind
              | Other (Text, Text)
-}

getOccNameDynamically ::
  forall t a.
  (Typeable t, NamedThing t, Data a) =>
  Proxy t ->
  a ->
  Maybe Text
getOccNameDynamically _ x =
  let my :: Maybe t
      my = cast x
   in fmap (T.pack . occNameString . getOccName) my

-- NOTE: a few data types used in Core has partial toConstr (Data.ByteString.ByteString)
-- or abstractConstr, which causes an exception or not inspectable expression
-- after conversion.
-- Many of them contain stringy information (usually name), so we extract the names
-- in such cases and do not proceed the conversion to the children of the node.

-- | Left: ordinary node, Right: extract and do not proceed to the children
getContent :: forall a. Data a => a -> (Text, Either Text (Maybe Text))
getContent x = (T.pack dtypName, evalue)
  where
    dtyp = dataTypeOf x
    dtypName = dataTypeName dtyp
    evalue
      | dtypName == "Data.ByteString.ByteString" =
          let mbs = cast x
           in Right (fmap decodeUtf8 mbs)
      -- unclear how to treat Bag
      | dtypName == "Bag" = Right Nothing
      | dtypName == "Class" = Right $ getOccNameDynamically (Proxy @Class) x
      -- unclear how to handle CoAxiom
      | dtypName == "CoAxiom" = Right Nothing
      -- unclear how to treat CoAxiomRule (coaxrProves is a function)
      | dtypName == "CoAxiomRule" = Right Nothing
      -- unclear how to treat CoercionHole (ch_ref :: IORef)
      | dtypName == "CoercionHole" = Right Nothing
      | dtypName == "ConLike" = Right $ getOccNameDynamically (Proxy @ConLike) x
      | dtypName == "DataCon" = Right $ getOccNameDynamically (Proxy @DataCon) x
      | dtypName == "FastString" =
          let my = cast x
           in Right (fmap (decodeUtf8 . SB.fromShort . fs_sbs) my)
      -- unclear how to treat HoleExprRef
      | dtypName == "HoleExprRef" = Right Nothing
      | dtypName == "ModuleName" =
          let my :: Maybe ModuleName = cast x
           in Right $ fmap (T.pack . moduleNameString) my
      | dtypName == "Name" = Right $ getOccNameDynamically (Proxy @Name) x
      | dtypName == "OccName" =
          let my :: Maybe OccName = cast x
           in Right $ fmap (T.pack . occNameString) my
      | dtypName == "PatSyn" = Right $ getOccNameDynamically (Proxy @PatSyn) x
      | dtypName == "RealSrcSpan" =
          let my :: Maybe RealSrcSpan = cast x
           in Right $ fmap (T.pack . show) my
      | dtypName == "SrcSpan" =
          let my :: Maybe SrcSpan = cast x
           in Right $ fmap (T.pack . show) my
      -- unclear how to treat TcEvBinds
      | dtypName == "TcEvBinds" = Right Nothing
      | dtypName == "TyCon" = Right $ getOccNameDynamically (Proxy @TyCon) x
      | dtypName == "Unit" =
          let my :: Maybe Unit = cast x
           in Right $ fmap (T.pack . unitString . toUnitId) my
      | dtypName == "Var" = Right $ getOccNameDynamically (Proxy @Var) x
      | otherwise = Left (T.pack (show (toConstr x)))

core2tree :: forall a. Data a => a -> Tree (Text, Text)
core2tree a =
  case getContent a of
    (typ, Left val) -> Node (typ, val) (getConst (gfoldl k z a))
    (typ, Right (Just val)) -> Node (typ, val) []
    (typ, Right Nothing) -> Node (typ, "#######") []
  where
    z _ = Const []
    k (Const acc) x = Const (acc ++ [core2tree x])

printCore :: ModGuts -> CoreM ConsoleReply
printCore guts = do
  let binds = mg_binds guts
      forest = fmap core2tree binds
  pure (ConsoleReplyCore forest)
