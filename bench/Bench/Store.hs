module Bench.Store where

import Data.Store
import Data.Word
import Data.Int

{-# INLINE read10b #-}
read10b :: Int -> Peek Int64
read10b = loop 0
    where 
        loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
            a1 <- peek :: Peek Int8
            a2 <- peek :: Peek Int8
            a3 <- peek :: Peek Int8
            a4 <- peek :: Peek Int8
            a5 <- peek :: Peek Int8
            a6 <- peek :: Peek Int8
            a7 <- peek :: Peek Int8
            a8 <- peek :: Peek Int8
            a9 <- peek :: Peek Int8
            a10 <- peek :: Peek Int8
            loop (s + fromIntegral a1 + fromIntegral a2 + fromIntegral a3 + fromIntegral a4
                    + fromIntegral a5 + fromIntegral a6 + fromIntegral a7 + fromIntegral a8
                    + fromIntegral a9 + fromIntegral a10) (n - 1)

{-# INLINE read12Int64PlusInt32 #-}
read12Int64PlusInt32 :: Int -> Peek Int64
read12Int64PlusInt32 = loop 0
    where 
        loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
            a1 <- peek :: Peek Int64
            a2 <- peek :: Peek Int64
            a3 <- peek :: Peek Int64
            a4 <- peek :: Peek Int64
            a5 <- peek :: Peek Int64
            a6 <- peek :: Peek Int64
            a7 <- peek :: Peek Int64
            a8 <- peek :: Peek Int64
            a9 <- peek :: Peek Int64
            a10 <- peek :: Peek Int64
            a11 <- peek :: Peek Int64
            a12 <- peek :: Peek Int64
            a13 <- peek :: Peek Int32
            loop (s + a1 + a2 + a3 + a4
                    + a5 + a6 + a7 + a8
                    + a9 + a10 + a11 + a12
                    + fromIntegral a13) (n - 1)

{-# INLINE getWord64N16Host #-}
getWord64N16Host :: Int -> Peek Word64
getWord64N16Host = loop 0
    where 
        loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
            s0 <- peek
            s1 <- peek
            s2 <- peek
            s3 <- peek
            s4 <- peek
            s5 <- peek
            s6 <- peek
            s7 <- peek
            s8 <- peek
            s9 <- peek
            s10 <- peek
            s11 <- peek
            s12 <- peek
            s13 <- peek
            s14 <- peek
            s15 <- peek
            loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15) (n-16)
    
{-# INLINE getWord8N16 #-}
getWord8N16 :: Int -> Peek Word8
getWord8N16 = loop 0
    where 
        loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
            s0 <- peek
            s1 <- peek
            s2 <- peek
            s3 <- peek
            s4 <- peek
            s5 <- peek
            s6 <- peek
            s7 <- peek
            s8 <- peek
            s9 <- peek
            s10 <- peek
            s11 <- peek
            s12 <- peek
            s13 <- peek
            s14 <- peek
            s15 <- peek
            loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15) (n-16)
            