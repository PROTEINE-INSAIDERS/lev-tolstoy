module Lev.Buffer where

import           Data.Word
import           Foreign.ForeignPtr

-- | Shared GC-managed read-only memory buffer.
-- Структуры буфера не очень интересны без дополнительной информации о типах.
-- возможно вместо них следует использовать чистые аргументы, а к структурам
-- перейти в типизированных интерфейсах

data Buffer = Buffer !(ForeignPtr Word8) -- base
                     !Int                -- offset
                     !Int                -- length
