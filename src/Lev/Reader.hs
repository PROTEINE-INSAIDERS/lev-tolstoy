{-# LANGUAGE DataKinds, RankNTypes, ScopedTypeVariables, GADTs,
             TypeApplications, PolyKinds, TypeFamilies  #-}

module Lev.Reader ( module Lev.Reader, module X ) where

import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import           Data.ByteString
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Typeable
import           Lev.Reader.FixedLength as X hiding ( Reader(..), bindReader, pureReader )
import qualified Lev.Reader.FixedLength as Fixed
import           Lev.Reader.Cursor as X
import           UnliftIO

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
    rf <*> ra = bindReader (`fmap` ra) rf

instance Monad (Reader c m) where
    {-# INLINE (>>=) #-}
    (>>=) = flip bindReader

data ReaderFailed c = ReaderFailed c String deriving (Show, Typeable)
instance (Show c, Typeable c) => Exception (ReaderFailed c)

instance (Monad m, Typeable c, Show c) => MonadFail (Reader c m) where
    {-# INLINE fail #-}
    fail msg = Reader $ \c _ -> return $ Fail $ toException $ ReaderFailed c msg

instance (MonadIO m) => MonadIO (Reader c m) where
    {-# INLINE liftIO #-}
    liftIO f = Reader $ \c k -> do
        a <- liftIO f
        k c a

{-# INLINE fixed #-}
fixed :: forall s c m a . ( KnownNat s, Consumable c, PrimMonad m ) => Fixed.Reader 0 s m a -> Reader c m a
fixed (Fixed.Reader f) = Reader $ \c0 k -> do
    let size = fromIntegral (natVal $ sing @s)
    consume c0 size $ \c1 addr -> f addr $ \a -> k c1 a

{-# INLINE readAhead #-}
readAhead :: ( Splittable c, PrimMonad m ) => Reader c m a -> Reader c m a
readAhead (Reader f) = Reader $ \c k -> 
    consumeSplit c undefined--(\c0 -> f c0 $ \c1 a -> return (c1, Done a)) 
                   (\c0 _ r -> case r of
                                  Done a -> k c0 a
                                  Fail e -> return (Fail e))

--TODO: подумать, как можно этот метод специализировать по ByteStringCursor (и нужно ли).
{-# INLINE slice #-}
slice :: ( Sliceable c, PrimMonad m ) => Int -> Reader c m ByteString
slice size = Reader $ \c k -> consumeSlice c size k
