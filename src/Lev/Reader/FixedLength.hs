{-# LANGUAGE CPP
           , DataKinds
           , RankNTypes
           , ScopedTypeVariables
           , TypeFamilies
           , TypeOperators 
           , TypeApplications
           , FlexibleContexts
  #-}

module Lev.Reader.FixedLength ( X.Result(..)
                         , module Lev.Reader.FixedLength
                         ) where

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
import           Data.Word
import           Foreign.ForeignPtr
import           Lev.Reader.Result as X
import           System.Endian
import           UnliftIO.Exception

newtype Reader (o :: Nat) (s :: Nat) m a = Reader 
    { runReader :: forall r . Addr -> (a -> m (Result r)) -> m (Result r) }

{-# INLINE pureReader #-}
pureReader :: a -> Reader o 0 m a
pureReader a = Reader $ \_ k -> k a

{-# INLINE bindReader #-}
bindReader :: ((oa + sa) ~ ob) => (a -> Reader ob sb m b) -> Reader oa sa m a -> Reader oa (sa + sb) m b
bindReader g (Reader f) = Reader $ \addr k -> f addr $ \a -> runReader (g a) addr k  

{-# INLINE (>>>=) #-}
(>>>=) :: ((oa + sa) ~ ob) => Reader oa sa m a -> (a -> Reader ob sb m b) -> Reader oa (sa + sb) m b
(>>>=) = flip bindReader

instance Functor (Reader (o :: Nat) (s :: Nat) m) where 
    {-# INLINABLE fmap #-}
    fmap f = bindReader (pureReader . f)

{-# INLINABLE readByteString #-}
-- TODO: Переименовать или вообще удалить. Для запуска статического ридера его надо сначала конвертировать в динамический, 
-- потом использовать специализированную запускалку.
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
        Fail e -> throwIO e

{-# INLINE skip #-}
skip :: forall s o m . (KnownNat o, PrimMonad m) => Reader o s m ()
skip = Reader $ \_ k -> k ()

-- TODO: DO NOT EXPOSE!! (otherwise introduce sizeof which is not safe though)
{-# INLINE prim #-}
prim :: forall o s m a . (KnownNat o, PrimMonad m, Prim a) => Reader o s m a 
prim = Reader $ \addr k -> readOffAddr (addr `plusAddr` off) 0 >>= k
    where
        off = fromIntegral $ natVal $ sing @o

#include "MachDeps.h"

#define PRIM(F,A,S) \
{-# INLINABLE F #-}; \
{-# SPECIALISE F :: (KnownNat o) => Reader o S IO A #-}; \
{-# SPECIALISE F :: (KnownNat o) => Reader o S (ST s) A #-}; \
F :: forall o m . (KnownNat o, PrimMonad m) => Reader o S m A; \
F = prim

PRIM(readWord8, Word8, SIZEOF_WORD8)
PRIM(readWord16, Word16, SIZEOF_WORD16)
PRIM(readWord32, Word32, SIZEOF_WORD32)
PRIM(readWord64, Word64, SIZEOF_WORD64)

PRIM(readInt8, Int8, SIZEOF_INT8)
PRIM(readInt16, Int16, SIZEOF_INT16)
PRIM(readInt32, Int32, SIZEOF_INT32)
PRIM(readInt64, Int64, SIZEOF_INT64)

#define PRIMW(F,A,S,E) \
{-# INLINABLE F #-}; \
{-# SPECIALISE F :: (KnownNat o) => Reader o S IO A #-}; \
{-# SPECIALISE F :: (KnownNat o) => Reader o S (ST s) A #-}; \
F :: forall o m . (KnownNat o, PrimMonad m) => Reader o S m A; \
F = E <$> prim

PRIMW(readWord16le, Word16, SIZEOF_WORD16, fromLE16)
PRIMW(readWord16be, Word16, SIZEOF_WORD16, fromBE16)

PRIMW(readWord32le, Word32, SIZEOF_WORD32, fromLE32)
PRIMW(readWord32be, Word32, SIZEOF_WORD32, fromBE32)

PRIMW(readWord64le, Word64, SIZEOF_WORD64, fromLE64)
PRIMW(readWord64be, Word64, SIZEOF_WORD64, fromBE64)

#define PRIMI(F,A,S,E) \
{-# INLINABLE F #-}; \
{-# SPECIALISE F :: (KnownNat o) => Reader o S IO A #-}; \
{-# SPECIALISE F :: (KnownNat o) => Reader o S (ST s) A #-}; \
F :: forall o m . (KnownNat o, PrimMonad m) => Reader o S m A; \
F = fromIntegral . E <$> prim

PRIMI(readInt16le, Int16, SIZEOF_INT16, fromLE16)
PRIMI(readInt16be, Int16, SIZEOF_INT16, fromBE16)

PRIMI(readInt32le, Int32, SIZEOF_INT32, fromLE32)
PRIMI(readInt32be, Int32, SIZEOF_INT32, fromBE32)

PRIMI(readInt64le, Int64, SIZEOF_INT64, fromLE64)
PRIMI(readInt64be, Int64, SIZEOF_INT64, fromBE64)
