module Bench.Lev.Reader.Dynamic where

import Data.Int
import Data.Word
import Lev.Reader.Dynamic
import Bench.Lev.Reader.Static as Static

{-# INLINE read12Int64PlusInt32 #-}
read12Int64PlusInt32 :: ( Cursor c ) => Reader c IO Int64
read12Int64PlusInt32 = readStatic Static.read12Int64PlusInt32

{-# INLINE getWord64N16Host #-}
getWord64N16Host :: (Cursor c) => Int -> Reader c IO Word64
getWord64N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop _ n = do 
            r <- readStatic Static.getWord64N16Host
            loop r (n-16)