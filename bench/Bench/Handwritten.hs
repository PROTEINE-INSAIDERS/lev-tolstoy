{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}

module Bench.Handwritten (
    read12Int64PlusInt32
  ) where

import           Control.Monad.Primitive
import           Data.ByteString
import           Data.ByteString.Internal
import           Data.Int
import           Foreign.ForeignPtr
import           GHC.Int
import           GHC.Prim
import           GHC.Ptr

{-# INLINE read12Int64PlusInt32 #-}
read12Int64PlusInt32 :: ByteString -> (Int64, ByteString)
read12Int64PlusInt32 buffer = case toForeignPtr buffer of
  (fbase, I# off, I# len) ->
    unsafeInlineIO $ withForeignPtr fbase $ \(Ptr base) -> do
      let !addr = plusAddr# base off
          !a1  = indexInt64OffAddr# addr 0#
          !a2  = indexInt64OffAddr# (plusAddr# addr  8#) 0#
          !a3  = indexInt64OffAddr# (plusAddr# addr 16#) 0#
          !a4  = indexInt64OffAddr# (plusAddr# addr 24#) 0#
          !a5  = indexInt64OffAddr# (plusAddr# addr 32#) 0#
          !a6  = indexInt64OffAddr# (plusAddr# addr 40#) 0#
          !a7  = indexInt64OffAddr# (plusAddr# addr 48#) 0#
          !a8  = indexInt64OffAddr# (plusAddr# addr 56#) 0#
          !a9  = indexInt64OffAddr# (plusAddr# addr 64#) 0#
          !a10 = indexInt64OffAddr# (plusAddr# addr 72#) 0#
          !a11 = indexInt64OffAddr# (plusAddr# addr 80#) 0#
          !a12 = indexInt64OffAddr# (plusAddr# addr 88#) 0#
          !a13 = indexInt32OffAddr# (plusAddr# addr 96#) 0#
      return ( I64# (a1 +# a2 +# a3 +# a4
               +# a5 +# a6 +# a7 +# a8
               +# a9 +# a10 +# a11 +# a12
               +# a13), fromForeignPtr fbase (I# (off +# 100#)) (I# (len -# 100#)) )
