module Bench.BinaryNano where

import           Data.Binary.Get as X
import           Data.ByteString as BS
import           Data.ByteString.Unsafe as B
import           Data.ByteString.Internal as B
import           Data.Int
import           Data.Word
import Data.Binary.Get.Internal as I
import Foreign.Storable
import Foreign.Ptr

read12Int64PlusInt32 :: Get Int64
read12Int64PlusInt32 = do
  a1 <- getInt64host
  a2 <- getInt64host
  a3 <- getInt64host
  a4 <- getInt64host
  a5 <- getInt64host
  a6 <- getInt64host
  a7 <- getInt64host
  a8 <- getInt64host
  a9 <- getInt64host
  a10 <- getInt64host
  a11 <- getInt64host
  a12 <- getInt64host
  a13 <- getInt32host
  return $ a1 + a2 + a3 + a4
         + a5 + a6 + a7 + a8
         + a9 + a10 + a11 + a12
         + fromIntegral a13

{-
{-# INLINE ff #-}
ff s = B.accursedUnutterablePerformIO $ B.unsafeUseAsCString s (peek . castPtr)

read12Int64PlusInt32 :: Get Int64
read12Int64PlusInt32 = (+) <$> (readN 8 ff) <*> (readN 8 ff) 
-}

{-# INLINE read12Int64PlusInt32 #-}
