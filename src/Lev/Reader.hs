{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}

module Lev.Reader
  ( Reader
  , bindReader
  , unitReader
  , runReaderWithByteString
  , runReaderWithForeignPtr
  , tryRead
  , readPrim
  , readWord8
  , readWord16
  , readWord32
  , readWord64
  , readInt8
  , readInt16Host
  , readInt32Host
  , readInt64Host
  , readByteString
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.ByteString
import           Data.ByteString.Internal
import           Data.Int
import           Data.Primitive
import           Data.Proxy
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           GHC.Exts
import           Lev.Layout
import Data.Either

-- * Utility functions

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Lev.Reader." ++ fun ++ ':':' ':msg

{-# NOINLINE moduleError #-}
moduleError :: String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)

data Reader m p (l :: Layout) a where
  StaticReader :: !(Addr -> m a) -> Reader m p ('StaticLayout o s) a
  DynamicReader :: !(forall r . DynamicReaderState p -> DynamicReaderCont m p a r -> m (DynamicReaderResult m p r)) -> Reader m p 'DynamicLayout a

type DynamicReaderState p = ( p, Addr, Int ) -- ^ (base pointer, current address, bytes remains in buffer)

data DynamicReaderResult m p r = Done  r
                               | Fetch !Addr !Int (DynamicReaderState p -> m (DynamicReaderResult m p r))
-- Fetch protocol:
-- Reader should only fetch when it reuqires more data than available in buffer.
-- Driver should olways return no less data than was requested by buffer.
-- Thus Fetch request during reading strict buffer indicates buffer exhaustion.

type DynamicReaderCont m p a r = DynamicReaderState p -> a -> m ( DynamicReaderResult m p r )

{-# INLINE bindReader #-}
bindReader :: forall m p la lb a b .
      ( BindLayoutInv la lb
      , Monad m
      , SingI la
      , SingI lb )
     => Reader m p la a
     -> (a -> Reader m p lb b)
     -> Reader m p (BindLayout la lb) b

bindReader (StaticReader fa) k = case (sing :: Sing lb) of
  SStaticLayout _ _ -> StaticReader $ \addr -> do
    a <- fa addr
    case k a of StaticReader fb -> fb addr
  SDynamicLayout -> DynamicReader $ \s k' -> case (sing :: Sing la) of
    SStaticLayout soff ssize -> do
      let off = fromIntegral (natVal soff) :: Int
          size =  fromIntegral (natVal ssize) :: Int
      case dynamicReader (off + size) $ const fa of 
        DynamicReader fa' -> fa' s $ \s' a -> case k a of (DynamicReader fb) -> fb s' k'

bindReader (DynamicReader fa) k = case (sing :: Sing lb) of
  SStaticLayout soff ssize -> DynamicReader $ \s k' ->
    fa s $ \(base, addr, remains) a -> case k a of
      (StaticReader fb) -> do
        let off =  fromIntegral (natVal soff)
            size = fromIntegral (natVal ssize)
            len = off + size
        if len <= remains
          then fb addr >>= k' (base, addr `plusAddr` len, remains - len)
          else return $ Fetch addr len $ \(base', addr', remains') -> fb addr' >>= k' (base', addr' `plusAddr` len, remains' - len)
  SDynamicLayout -> DynamicReader $ \s k' ->
    fa s $ \s' a -> case k a of (DynamicReader fb) -> fb s' k'

{-# INLINE unitReader #-}
unitReader :: (Applicative m) => a -> Reader m p ('StaticLayout o 0) a
unitReader = StaticReader . const . pure

{-# INLINE runReaderWithByteString #-}
runReaderWithByteString :: forall l a . ( SingI (l :: Layout ) )
                        => Reader IO (ForeignPtr Word8) l a
                        -> ByteString
                        -> IO (a, ByteString)
runReaderWithByteString r bs = do
  let (bPtr, bOff, bSize) = toForeignPtr bs
  (res, bytesRead) <- runReaderWithForeignPtr r bPtr bOff bSize
  return (res, fromForeignPtr bPtr (bOff + bytesRead) (bSize - bytesRead))

{-# INLINE runReaderWithForeignPtr #-}
runReaderWithForeignPtr :: forall l a . ( SingI (l :: Layout ) )
                        => Reader IO (ForeignPtr Word8) l a -- ^ reader to run
                        -> ForeignPtr Word8 -- ^ pointer to memory buffer
                        -> Int -- ^ offset in buffer
                        -> Int -- ^ available size
                        -> IO (a, Int) -- ^ (value, bytes read from buffer)
runReaderWithForeignPtr (StaticReader f) bPtr bOff bSize =
  case (sing :: Sing l) of
    SStaticLayout soff ssize -> do
      let loff = fromIntegral (natVal soff)
          rReq = loff + fromIntegral (natVal ssize)
      when (bSize < rReq) $
        moduleError "runReader" $ "Buffer size = " ++ show bSize ++
                                  ", bytes required = " ++ show rReq
      withForeignPtr bPtr $ \(Ptr bAddr) -> do
        result <- f (Addr bAddr `plusAddr` (loff + bOff))
        return (result, rReq)
runReaderWithForeignPtr (DynamicReader f) bPtr bOff bSize =
  withForeignPtr bPtr $ \(Ptr bAddr) -> do
    res <- f (bPtr, Addr bAddr `plusAddr` bOff, bSize) done
    case res of
      Done x -> return x
      Fetch addr req _ ->
        moduleError "runReader" $ "Buffer size = " ++ show bSize ++
                                  ", bytes required = " ++ show (req + (addr `minusAddr` Addr bAddr) - bOff)
    where
      done (_, _, remains) result = return $ Done (result, bSize - remains)

{-# INLINE readPrim #-}
readPrim :: forall m p o a . ( PrimMonad m , Prim a, KnownNat o ) => Reader m p ('StaticLayout o (SizeOf a)) a
readPrim = StaticReader $ \addr -> readOffAddr (addr `plusAddr` off) 0
  where off = fromIntegral $ natVal (Proxy :: Proxy o)

type PrimReader a = forall m p o . ( PrimMonad m , Prim a, KnownNat o ) => Reader m p ('StaticLayout o (SizeOf a)) a

{-# INLINE readWord8 #-}
readWord8 :: PrimReader Word8
readWord8 = readPrim

{-# INLINE readWord16 #-}
readWord16 :: PrimReader Word16
readWord16 = readPrim

{-# INLINE readWord32 #-}
readWord32 :: PrimReader Word32
readWord32 = readPrim

{-# INLINE readWord64 #-}
readWord64 :: PrimReader Word64
readWord64 = readPrim

{-# INLINE readInt8 #-}
readInt8 :: PrimReader Int8
readInt8 = readPrim

{-# INLINE readInt16Host #-}
readInt16Host :: PrimReader Int16
readInt16Host = readPrim

{-# INLINE readInt32Host #-}
readInt32Host :: PrimReader Int32
readInt32Host = readPrim

{-# INLINE readInt64Host #-}
readInt64Host :: PrimReader Int64
readInt64Host = readPrim

{-# INLINE dynamicReader #-}
dynamicReader :: forall m p a . (Monad m) => Int -> (p -> Addr -> m a) -> Reader m p 'DynamicLayout a
dynamicReader len f = DynamicReader $ \(base, addr, remains) k ->
  if len <= remains
    then f base addr >>= k (base, addr `plusAddr` len, remains - len)
    else return $ Fetch addr len $ \(base', addr', remains') -> f base' addr' >>= k (base', addr' `plusAddr` len, remains' - len)

{-# INLINE readByteString #-}
readByteString :: Int -> Reader IO (ForeignPtr Word8) 'DynamicLayout ByteString
readByteString len = dynamicReader len $ \base addr -> do
  let !(Ptr baseAddr) = unsafeForeignPtrToPtr base
  return $ fromForeignPtr base (addr `minusAddr` Addr baseAddr) len

{-# INLINE tryRead #-}
tryRead :: Reader m (ForeignPtr Word8) 'DynamicLayout (Either e a) -> Reader m (ForeignPtr Word8) 'DynamicLayout (Either e a)
tryRead (DynamicReader f) = DynamicReader $ \s k ->
  f s $ \s' a -> k (if isRight a then s' else s) a
