module Bench.Lev.Reader.Dynamic where

import Data.Int
import Lev.Reader.Dynamic
import Bench.Lev.Reader.Static as Static

{-# INLINE read12Int64PlusInt32 #-}
read12Int64PlusInt32 :: Reader IO p c Int64
read12Int64PlusInt32 = static Static.read12Int64PlusInt32