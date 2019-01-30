{-# LANGUAGE DataKinds, NoImplicitPrelude, RebindableSyntax #-}

module Bench.Lev.Reader.Static1 where

import Data.Int
import Lev.Reader.Static1
import           Prelude            hiding (Monad (..))

{-# INLINE read12Int64PlusInt32 #-}
read12Int64PlusInt32 :: Reader 0 100 IO Int64
read12Int64PlusInt32 = 
    readInt64 >>>= \a1 ->
    readInt64 >>>= \a2 ->
    readInt64 >>>= \a3 ->
    readInt64 >>>= \a4 ->
    readInt64 >>>= \a5 ->
    readInt64 >>>= \a6 ->
    readInt64 >>>= \a7 ->
    readInt64 >>>= \a8 ->
    readInt64 >>>= \a9 ->
    readInt64 >>>= \a10 ->
    readInt64 >>>= \a11 ->
    readInt64 >>>= \a12 ->
    readInt32 >>>= \a13 ->
    pureReader (a1 + a2 + a3 + a4
            + a5 + a6 + a7 + a8
            + a9 + a10 + a11 + a12
            + fromIntegral a13)
