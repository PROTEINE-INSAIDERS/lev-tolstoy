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
--   Once cursor passed to this function it no longer guaranteed to be valid
--   and sould not be used for repeatable reads.
class Consumable c where
    consume :: (PrimMonad m) => c -> Int -> (c -> Addr -> m (Result a)) -> m (Result a)

-- | A ByteString slice can be created. Once sliced, ByteString can be passed of reader monad.
-- However slicing may prevent underlying buffer from being garbage collected or may involve data copying.
class Sliceable c where
    consumeSlice :: (PrimMonad m) => c -> Int -> (c -> ByteString -> m (Result a)) -> m (Result a)

-- | Cursor can be splitted.
-- Consume function will be called at the current position. Then continuation will be invoked
-- with a consume function's result and two cursors. One pointing to the split point position, 
-- other - to the new position. Continuation is free to choose which cursor to use.

-- | TODO: придумать, как правильно реализовать работу с курсорами.
-- a. если сделать split буквально: m (c, c), то при использовании стриминга 
--    после выполнения split уже не будет возможности освободить буферы.
-- b. Варианты createBookmark, deleteBookmark, moveTo(bookmark) подразумевают либо
--    использование мутабельных структур, либо дополнительного типа Bookmark.
--    Комбинаторы типа readAhead должны гарантировать удаление закладки (как это
--    скажется на скорости их работы?)
-- c. Continuation passing. При отказе от мутабельности возникают проблемы с обновлением
--    состояния курсора. Курсор надо будет добавить в результат ридера, причем это будет сдвинутый 
--    курсор, из которого надо будет восстанавливать закладку на основании текущего курсора.
--    Также придётся внести изменения в сигнатуры consume и consumeSlice.
class Splittable c where 
    consumeSplit :: (PrimMonad m) => c -> (c -> m (c, Result a)) -> (c -> c -> Result a -> m (Result b)) -> m (Result b)