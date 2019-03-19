module Bench.Binary (
    runBinaryGetStrict
  , read12Int64PlusInt32
  , read4Strings
  , getWord64N16Host
  , module X
  ) where

import           Data.Binary.Get as X
import           Data.ByteString as BS
import           Data.Int
import           Data.Word

{-# INLINE runBinaryGetStrict #-}
runBinaryGetStrict :: Get a -> ByteString -> (a, ByteString)
runBinaryGetStrict g = feed (runGetIncremental g) . Just
  where
    feed (Done s _ a) _ = (a, s)
    feed (Partial f) s = feed (f s) Nothing
    feed (Fail _ pos msg) _ = error $ "Bench.Binary.runBinaryGetStrict failed at position "
                                   ++ show pos ++ " with message : " ++ msg

{-# INLINE read12Int64PlusInt32 #-}
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

read4Strings :: Get Int
read4Strings = do
  a1 <- getWord8
  b1 <- getByteString (fromIntegral a1)
  a2 <- getWord8
  b2 <- getByteString (fromIntegral a2)
  a3 <- getWord8
  b3 <- getByteString (fromIntegral a3)
  a4 <- getWord8
  b4 <- getByteString (fromIntegral a4)
  return (BS.length b1 + BS.length b2 + BS.length b3 + BS.length b4)
{-# INLINE read4Strings #-}

{-# INLINE getWord64N16Host #-}
getWord64N16Host :: Int -> Get Word64
getWord64N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64host
          s1 <- getWord64host
          s2 <- getWord64host
          s3 <- getWord64host
          s4 <- getWord64host
          s5 <- getWord64host
          s6 <- getWord64host
          s7 <- getWord64host
          s8 <- getWord64host
          s9 <- getWord64host
          s10 <- getWord64host
          s11 <- getWord64host
          s12 <- getWord64host
          s13 <- getWord64host
          s14 <- getWord64host
          s15 <- getWord64host
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15) (n-16)
