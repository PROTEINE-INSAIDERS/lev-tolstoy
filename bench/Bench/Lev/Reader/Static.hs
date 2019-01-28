{-# LANGUAGE DataKinds         #-}

module Bench.Lev.Reader.Static where

import Lev.Reader.Static
import Data.Int
    
{-# INLINE read12Int64PlusInt32 #-}
read12Int64PlusInt32 :: Reader 0 100 IO Int64
read12Int64PlusInt32 = 
    readInt64 `bindReader` \ a1 ->
    readInt64 `bindReader` \ a2 ->
    readInt64 `bindReader` \ a3 ->
    readInt64 `bindReader` \ a4 ->
    readInt64 `bindReader` \ a5 ->
    readInt64 `bindReader` \ a6 ->
    readInt64 `bindReader` \ a7 ->
    readInt64 `bindReader` \ a8 ->
    readInt64 `bindReader` \ a9 ->
    readInt64 `bindReader` \ a10 ->
    readInt64 `bindReader` \ a11 ->
    readInt64 `bindReader` \ a12 ->
    readInt32 `bindReader` \ a13 ->
    pureReader (a1 + a2 + a3 + a4
            + a5 + a6 + a7 + a8
            + a9 + a10 + a11 + a12
            + fromIntegral a13)
