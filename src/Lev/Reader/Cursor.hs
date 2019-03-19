module Lev.Reader.Cursor
    ( X.Result(..)
    , module Lev.Reader.Cursor
    )
where

import           Control.Monad.Primitive
import           Data.Primitive
import           Data.ByteString
import           Lev.Reader.Result             as X

class Cursor c where
    consume :: (PrimMonad m) => c -> Int -> (c -> Addr -> m (Result a)) -> m (Result a)

class (Cursor c) => ConsumeBytestring c where
    consumeBytestring :: (PrimMonad m) => c -> Int -> (c -> ByteString -> m (Result a)) -> m (Result a) 