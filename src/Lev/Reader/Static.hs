{-# LANGUAGE CPP
           , DataKinds
           , KindSignatures
           , Rank2Types
           , ScopedTypeVariables
           , TypeFamilies
           , TypeOperators 
           , TypeApplications
#-}

module Lev.Reader.Static where

import           Control.Exception
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Primitive
import           Data.Singletons
import           Data.Singletons.Prelude.Num
import           Data.Singletons.TypeLits
import           Data.Typeable
import           Data.Word

#include "MachDeps.h"

data Result a = Done {-# UNPACK #-} !a
              | Fail {-# UNPACK #-} !SomeException deriving (Show, Typeable)

newtype Reader (o :: Nat) (s :: Nat) m a = Reader 
    { runReader :: forall r . Addr -> (a -> m (Result r)) -> m (Result r) }

{-# INLINABLE pureReader #-}
pureReader :: a -> Reader o 0 m a
pureReader a = Reader $ \addr k -> k a

{-# INLINABLE bindReader #-}
bindReader :: ((oa + sa) ~ ob) => Reader oa sa m a -> (a -> Reader ob sb m b) -> Reader oa (sa + sb) m b
bindReader (Reader f) k = Reader $ \addr k -> do
    undefined

-- TODO: DO NOT EXPOSE!! (otherwise introduce sizeof which is not safe though)
{-# INLINE readPrim #-}
readPrim :: forall o s m a . (KnownNat o, PrimMonad m, Prim a) => Reader o s m a 
readPrim = Reader $ \addr k -> readOffAddr (addr `plusAddr` off) 0 >>= k
    where
        off = fromIntegral $ natVal $ sing @o

{-# INLINABLE readWord8 #-}
{-# SPECIALISE readWord8 :: (KnownNat o) => Reader o SIZEOF_WORD8 IO Word8 #-}
{-# SPECIALISE readWord8 :: (KnownNat o) => Reader o SIZEOF_WORD8 (ST s) Word8 #-}
readWord8 :: (KnownNat o, PrimMonad m) => Reader o SIZEOF_WORD8 m Word8
readWord8 = readPrim
