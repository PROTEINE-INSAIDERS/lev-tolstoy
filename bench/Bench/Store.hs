module Bench.Store where

import Data.Store
import Data.Word

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
    