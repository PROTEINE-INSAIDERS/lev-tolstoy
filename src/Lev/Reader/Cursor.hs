module Lev.Reader.Cursor
    ( X.Result(..)
    , module Lev.Reader.Cursor
    )
where

import           Control.Monad.Primitive
import           Data.Primitive
import           Data.ByteString
import           Lev.Reader.Result             as X

-- TODO: придумать как возвращать байтсринги (возможно, в этот класс следует добавить метод, либо в state добавить поле). 
class Cursor c where
    consume :: (PrimMonad m) => c -> Int -> (c -> Addr -> m (Result a)) -> m (Result a)

class (Cursor c) => ByteStringCursor c where
    readBytestring :: (PrimMonad m) => c -> Int -> (c -> ByteString -> m (Result a)) -> m (Result a) 