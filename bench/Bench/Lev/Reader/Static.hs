{-# LANGUAGE DataKinds, RebindableSyntax #-}

module Bench.Lev.Reader.Static where

import Data.Int
import Data.Word
import Lev.Reader.Static.Syntax
import           Prelude            hiding (Monad (..))

{-# INLINE read12Int64PlusInt32 #-}
read12Int64PlusInt32 :: Reader 0 100 IO Int64
read12Int64PlusInt32 = do
    a1 <- readInt64
    a2 <- readInt64
    a3 <- readInt64
    a4 <- readInt64
    a5 <- readInt64
    a6 <- readInt64
    a7 <- readInt64
    a8 <- readInt64
    a9 <- readInt64
    a10 <- readInt64
    a11 <- readInt64
    a12 <- readInt64
    a13 <- readInt32
    return (a1 + a2 + a3 + a4
            + a5 + a6 + a7 + a8
            + a9 + a10 + a11 + a12
            + fromIntegral a13)

{-# INLINE getWord64N16Host #-}
getWord64N16Host :: Reader 0 128 IO Word64
getWord64N16Host = do
    s0 <- readWord64
    s1 <- readWord64
    s2 <- readWord64
    s3 <- readWord64
    s4 <- readWord64
    s5 <- readWord64
    s6 <- readWord64
    s7 <- readWord64
    s8 <- readWord64
    s9 <- readWord64
    s10 <- readWord64
    s11 <- readWord64
    s12 <- readWord64
    s13 <- readWord64
    s14 <- readWord64
    s15 <- readWord64
    return (s0+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15)
    