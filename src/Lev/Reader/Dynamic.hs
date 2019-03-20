{-# LANGUAGE DataKinds, RankNTypes, ScopedTypeVariables, GADTs,
             TypeApplications, PolyKinds  #-}

module Lev.Reader.Dynamic ( X.Cursor, module Lev.Reader.Dynamic ) where

import           Control.Monad.Primitive
import           Data.ByteString
import           Data.Singletons
import           Data.Singletons.TypeLits
import qualified Lev.Reader.Static as Static 
import           Lev.Reader.Cursor as X

newtype Reader c m a = Reader { runReader :: forall r . c -> (c -> a -> m (Result r)) -> m (Result r) }

{-# INLINE pureReader #-}
pureReader :: a -> Reader c m a
pureReader a = Reader $ \c k -> k c a 

{-# INLINE bindReader #-}
bindReader :: (a -> Reader c m b) -> Reader c m a -> Reader c m b
bindReader g (Reader f) = Reader $ \c0 k -> f c0 $ \c1 a -> runReader (g a) c1 k  

instance Functor (Reader c m) where
    {-# INLINE fmap #-}
    fmap f = bindReader (pureReader . f)

instance Applicative (Reader c m) where
    {-# INLINE pure #-}
    pure = pureReader
    {-# INLINE (<*>) #-}
    rf <*> ra = bindReader (flip fmap ra) rf

instance Monad (Reader c m) where
    {-# INLINE (>>=) #-}
    (>>=) = flip bindReader

{-# INLINE readByteString #-}
readByteString :: ( ConsumeBytestring c, PrimMonad m ) => Int -> Reader c m ByteString
readByteString size = Reader $ \c k -> consumeBytestring c size k

{-# INLINE readStatic #-}
readStatic :: forall s c m a . ( KnownNat s, Cursor c, PrimMonad m ) => Static.Reader 0 s m a -> Reader c m a
readStatic (Static.Reader f) = Reader $ \c0 k -> do 
    let size = fromIntegral (natVal $ sing @s)
    consume c0 size $ \c1 addr -> f addr $ \a -> k c1 a
