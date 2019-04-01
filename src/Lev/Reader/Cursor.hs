module Lev.Reader.Cursor
    ( X.Result(..)
    , module Lev.Reader.Cursor
    )
where

import           Control.Monad.Primitive
import           Data.Primitive
import           Data.ByteString
import           Lev.Reader.Result             as X

-- | Number of bytes can be consumed from the input stream.
--   Once bytes consumed cursor not guaranteed to be valid any more
--   and sould not be used for repeatable reads.
class Consumable c where
    consume :: (PrimMonad m) => c -> Int -> (c -> Addr -> m (Result a)) -> m (Result a)

-- | A ByteString slice can be created. Once sliced, ByteString can be passed of reader monad.
-- However slicing may prevent underlying buffer from being released or may involve data being copied.
class Sliceable c where
    consumeSlice :: (PrimMonad m) => c -> Int -> (c -> ByteString -> m (Result a)) -> m (Result a)

-- | Cursor can be splitted.
-- Consume function will be called at the current position. Then continuation will be invoked
-- with a consume function's result and two cursors. One pointing to the split point position, 
-- and other to the new position. Continuation free to choose which cursor to use.
class Splittable c where 
    consumeSplit :: (PrimMonad m) => c -> Int -> (c -> Addr -> m (Result a)) -> (c -> c -> Result a -> m (Result b)) -> m (Result b)