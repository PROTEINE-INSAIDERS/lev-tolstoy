{-# LANGUAGE CPP
           , DataKinds
           , KindSignatures
           , RankNTypes
           , ScopedTypeVariables
           , TypeFamilies
           , TypeOperators 
           , TypeApplications
           , FlexibleContexts
  #-}

module Lev.Reader.Static where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.ByteString
import           Data.ByteString.Internal
import           Data.Int
import           Data.Primitive
import           Data.Primitive.Ptr
import           Data.Singletons
import           Data.Singletons.Prelude.Num
import           Data.Singletons.TypeLits
import           Data.Typeable
import           Data.Word
import           Foreign.ForeignPtr

data Result a = Done !a
              | Fail !SomeException deriving (Show, Typeable)

newtype Reader (o :: Nat) (s :: Nat) m a = Reader 
    { runReader :: forall r . Addr -> (a -> m (Result r)) -> m (Result r) }

{-# INLINABLE pureReader #-}
pureReader :: a -> Reader o 0 m a
pureReader a = Reader $ \_ k -> k a

{-# INLINABLE bindReader #-}
bindReader :: ((oa + sa) ~ ob) => (a -> Reader ob sb m b) -> Reader oa sa m a -> Reader oa (sa + sb) m b
bindReader g (Reader f) = Reader $ \addr k -> f addr $ \a -> runReader (g a) addr k  

{-# INLINABLE (>>>=) #-}
(>>>=) :: ((oa + sa) ~ ob) => Reader oa sa m a -> (a -> Reader ob sb m b) -> Reader oa (sa + sb) m b
(>>>=) = flip bindReader

{-# INLINABLE readByteString #-}
-- TODO: переименовать. 
readByteString :: forall o s a . ( KnownNat o, KnownNat (o + s) ) => Reader o s IO a -> ByteString -> IO (a, ByteString)
readByteString (Reader f) bs = do
    let (bPtr, bOff, bSize) = toForeignPtr bs
        rOff = fromIntegral (natVal $ sing @o)
        rReq = fromIntegral (natVal $ sing @(o + s))
    when (bSize < rReq) $ error "bSize < rReq"
    res <- withForeignPtr bPtr $ \(Ptr bAddr) ->
        f (Addr bAddr `plusAddr` (rOff + bOff)) (return . Done)
    case res  of 
        Done a -> return (a, fromForeignPtr bPtr (bOff + rReq) (bSize - rReq))
        Fail e -> error "fail"

-- TODO: DO NOT EXPOSE!! (otherwise introduce sizeof which is not safe though)
{-# INLINE readPrim #-}
readPrim :: forall o s m a . (KnownNat o, PrimMonad m, Prim a) => Reader o s m a 
readPrim = Reader $ \addr k -> readOffAddr (addr `plusAddr` off) 0 >>= k
    where
        off = fromIntegral $ natVal $ sing @o

#include "MachDeps.h"

#define READ_PRIM(F,A,S) \
{-# INLINABLE F #-}; \
{-# SPECIALISE F :: (KnownNat o) => Reader o S IO A #-}; \
{-# SPECIALISE F :: (KnownNat o) => Reader o S (ST s) A #-}; \
F :: forall o m . (KnownNat o, PrimMonad m) => Reader o S m A; \
F = readPrim

READ_PRIM(readWord8, Word8, SIZEOF_WORD8)
READ_PRIM(readWord16, Word16, SIZEOF_WORD16)
READ_PRIM(readWord32, Word32, SIZEOF_WORD32)
READ_PRIM(readWord64, Word64, SIZEOF_WORD64)

READ_PRIM(readInt8, Int8, SIZEOF_INT8)
READ_PRIM(readInt16, Int16, SIZEOF_INT16)
READ_PRIM(readInt32, Int32, SIZEOF_INT32)
READ_PRIM(readInt64, Int64, SIZEOF_INT64)