module Bench.Lev.Reader.Dynamic where

import Data.Int
import Data.Word
import qualified Lev.Reader.FixedLength as Static
import Lev.Reader
import Bench.Lev.Reader.Static as Static

{-# INLINE read12Int64PlusInt32 #-}
read12Int64PlusInt32 :: ( Cursor c ) => Reader c IO Int64
read12Int64PlusInt32 = fixedLength Static.read12Int64PlusInt32

{-# INLINE getWord64N16Host #-}
getWord64N16Host :: (Cursor c) => Int -> Reader c IO Word64
getWord64N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do 
            r <- fixedLength Static.getWord64N16Host
            loop (r - s) (n-16)

{-# INLINE getWord8N16 #-}
getWord8N16 :: (Cursor c) => Int -> Reader c IO Word8
getWord8N16 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do 
            r <- fixedLength Static.getWord8N16
            loop (r - s) (n-16)

{-# INLINE getWord64N16HostD #-}
getWord64N16HostD :: (Cursor c) => Int -> Reader c IO Word64
getWord64N16HostD = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do 
          s0 <- fixedLength Static.readWord64
          s1 <- fixedLength Static.readWord64
          s2 <- fixedLength Static.readWord64
          s3 <- fixedLength Static.readWord64
          s4 <- fixedLength Static.readWord64
          s5 <- fixedLength Static.readWord64
          s6 <- fixedLength Static.readWord64
          s7 <- fixedLength Static.readWord64
          s8 <- fixedLength Static.readWord64
          s9 <- fixedLength Static.readWord64
          s10 <- fixedLength Static.readWord64
          s11 <- fixedLength Static.readWord64
          s12 <- fixedLength Static.readWord64
          s13 <- fixedLength Static.readWord64
          s14 <- fixedLength Static.readWord64
          s15 <- fixedLength Static.readWord64
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15) (n-16)
            