module GHCSpecter.Util.Map
  ( IsKey (..),
    BiIntMap,
    mkEmptyBiIntMap,
    forwardLookup,
    backwardLookup,
    insert,
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import GHC.Generics (Generic)

class IsKey k where
  fromKey :: k -> Int
  toKey :: Int -> k

data BiIntMap k v = BiIntMap
  { forwardMap :: IntMap v
  , backwardMap :: Map v k
  }
  deriving (Generic)

instance (FromJSON k, FromJSON v, FromJSONKey v, Ord v) => FromJSON (BiIntMap k v)

instance (ToJSON k, ToJSON v, ToJSONKey v) => ToJSON (BiIntMap k v)

mkEmptyBiIntMap :: (IsKey k, Ord v) => BiIntMap k v
mkEmptyBiIntMap = BiIntMap mempty mempty

forwardLookup :: (IsKey k) => k -> BiIntMap k v -> Maybe v
forwardLookup k m = IM.lookup (fromKey k) (forwardMap m)

backwardLookup :: (IsKey k, Ord v) => v -> BiIntMap k v -> Maybe k
backwardLookup v m = M.lookup v (backwardMap m)

insert :: (IsKey k, Ord v) => (k, v) -> BiIntMap k v -> BiIntMap k v
insert (k, v) m =
  let fwdmap = forwardMap m
      bwdmap = backwardMap m
   in m
        { forwardMap = IM.insert (fromKey k) v fwdmap
        , backwardMap = M.insert v k bwdmap
        }
