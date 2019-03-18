{-# LANGUAGE DataKinds, RankNTypes, ScopedTypeVariables, GADTs,
             TypeApplications, PolyKinds  #-}

module Lev.Reader.Dynamic ( X.Cursor, module Lev.Reader.Dynamic ) where

import            Control.Monad.Primitive
import           Data.Singletons
import           Data.Singletons.TypeLits
import qualified Lev.Reader.Static as Static 
import           Lev.Reader.Cursor as X

newtype Reader c m a = Reader { runReader :: forall r . c -> (c -> a -> m (Result r)) -> m (Result r) }

{-# INLINABLE static #-}
static :: forall s c m a . ( KnownNat s, Cursor c, PrimMonad m ) => Static.Reader 0 s m a -> Reader c m a
static (Static.Reader f) = Reader $ \c0 k -> do 
    let size = fromIntegral (natVal $ sing @s)
    consume c0 size $ \c1 addr -> f addr $ \a -> k c1 a