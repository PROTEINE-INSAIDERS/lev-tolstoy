module Bench.Lev.Reader.Dynamic1 where

import Data.Int
import Lev.Reader.Dynamic1
import Bench.Lev.Reader.Static as Static

{-# INLINE read12Int64PlusInt32 #-}
read12Int64PlusInt32 :: Reader IO Int64
read12Int64PlusInt32 = static Static.read12Int64PlusInt32