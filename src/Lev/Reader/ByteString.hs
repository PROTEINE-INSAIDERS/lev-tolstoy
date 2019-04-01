{-# LANGUAGE BangPatterns, TypeFamilies #-}

-- TODO: реэкспортировать Reader. Подразумевается, что пользователь импортирует Lev.Reader.ByteString и читает байтстринги.  
module Lev.Reader.ByteString ( module Lev.Reader.ByteString
                             , module X
                             ) where

import           Data.ByteString
import           Data.ByteString.Internal
import           Data.Primitive
import           Data.Primitive.Ptr
import           Data.Typeable
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           Lev.Reader.Cursor as X
import           Lev.Reader as X
import           Lev.Readable as X
import           UnliftIO.Exception

data ByteStringCursor = ByteStringCursor !(ForeignPtr Word8) !Addr !Addr deriving (Show, Typeable)

data ByteStringError = ByteStringOverflow !Addr !Addr deriving (Show, Typeable)

instance Exception ByteStringError

instance Consumable ByteStringCursor where
    {-# INLINE consume #-}
    consume (ByteStringCursor bPtr currentAddr maxAddr) size k =
        let nextAddr = currentAddr `plusAddr` size
         in if nextAddr <= maxAddr
                then k (ByteStringCursor bPtr nextAddr maxAddr) currentAddr
                else
                    return $ Fail
                        (toException $ ByteStringOverflow nextAddr maxAddr)

instance Sliceable ByteStringCursor where
    {-# INLINE consumeSlice #-}
    consumeSlice cursor size k = 
        consume cursor size $ \c@(ByteStringCursor bPtr _ _) addr -> do
            let !(Ptr !bAddr) = unsafeForeignPtrToPtr bPtr -- эта функция всегда вызывается из withForeignPtr
                off = addr `minusAddr` Addr bAddr
            k c $ fromForeignPtr bPtr off size

instance Splittable ByteStringCursor where
--     consumeSplit :: (PrimMonad m) => c -> Int -> (c -> Addr -> m (Result a)) -> (c -> c -> Result a -> m (Result b)) -> m (Result b)
    consumeSplit cursorSplit size f k = undefined 

-- todo: remove!
{-# INLINE runByteString #-}
runByteString :: Reader ByteStringCursor IO a -> ByteString -> IO (a, ByteString)
runByteString (Reader f) bs = do 
    let (bPtr, bOff, bSize) = toForeignPtr bs
    withForeignPtr bPtr $ \(Ptr bAddr) -> do
        let startAddr = Addr bAddr `plusAddr` bOff
            maxAddr = startAddr `plusAddr` bSize
        res <- f (ByteStringCursor bPtr startAddr maxAddr) (\(ByteStringCursor _ e _) a -> return $ Done (a, e))
        case res of 
            Done (a, endAddr) -> do
                let newOff = endAddr `minusAddr` Addr bAddr
                    newSize = bSize - (newOff - bOff)
                return (a, fromForeignPtr bPtr newOff newSize) 
            Fail e -> throwIO e
                        

instance Readable ByteString where 
    type ReaderConsumable ByteString = ByteStringCursor
    type ReaderMonad ByteString = IO
    {-# INLINE readWith #-}
    readWith reader byteString =  fst <$> runByteString reader byteString