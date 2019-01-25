{-# LANGUAGE Rank2Types #-}

module Lev.Reader.Static where

import           Control.Exception
import           Control.Monad.Primitive
import           Data.Primitive
import           Data.Word

data Result a = Done {-# UNPACK #-} !a
              | Fail {-# UNPACK #-} !SomeException

newtype Reader m a = Reader { runReader :: forall b . Addr -> (Addr -> b -> m (Result a)) -> m a }

{-# INLINABLE readWord8 #-}
readWord8 :: (PrimMonad m) => Reader m Word8
readWord8 = Reader $ \addr k -> do
    a <- readOffAddr addr 0
    return a
