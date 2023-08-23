{- ORMOLU_DISABLE -}
{-# LINE 1 "StorableInstances.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util.Orphans () where

import Data.String (IsString (..))
import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CFloat)
import Foreign.Storable (Storable (..))
import ImGui
import ImGui.ImVec2.Implementation
import System.IO.Unsafe (unsafePerformIO)

instance IsString CString where
  fromString s = unsafePerformIO $ newCString s

instance Storable ImVec2 where
  sizeOf _ = (8)
{-# LINE 15 "StorableInstances.hsc" #-}
  alignment _ = alignment (undefined :: CFloat)
  poke p v = do
    x <- imVec2_x_get v
    y <- imVec2_y_get v
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ x
{-# LINE 20 "StorableInstances.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) p $ y
{-# LINE 21 "StorableInstances.hsc" #-}
  peek p = do
    x <- ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
{-# LINE 23 "StorableInstances.hsc" #-}
    y <- ((\hsc_ptr -> peekByteOff hsc_ptr 4) p)
{-# LINE 24 "StorableInstances.hsc" #-}
    newImVec2 x y


