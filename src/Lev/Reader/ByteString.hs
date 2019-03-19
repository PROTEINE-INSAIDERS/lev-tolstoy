module Lev.Reader.ByteString where

import           Data.ByteString
import           Data.ByteString.Internal
import           Data.Primitive
import           Data.Primitive.Ptr
import           Data.Typeable
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           Lev.Reader.Cursor
import           Lev.Reader.Dynamic
import           UnliftIO.Exception

data ByteStringCursor = ByteStringCursor !(ForeignPtr Word8) !Addr !Addr

data ByteStringError = ByteStringOverflow !Addr !Addr deriving ( Show, Typeable )

instance Exception ByteStringError

instance Cursor ByteStringCursor where
    {-# INLINE consume #-}
    -- consume :: (PrimMonad m) => ByteStringCursor -> Int -> (ByteStringCursor -> Addr -> m (Result a)) -> m (Result a)
    consume (ByteStringCursor bPtr currentAddr maxAddr) size k =
        let nextAddr = currentAddr `plusAddr` size
         in if nextAddr <= maxAddr
                then k (ByteStringCursor bPtr nextAddr maxAddr) currentAddr
                else
                    return $ Fail
                        (toException $ ByteStringOverflow nextAddr maxAddr)

instance ConsumeBytestring ByteStringCursor where
    -- consumeBytestring :: (PrimMonad m) => ByteStringCursor -> Int -> (ByteStringCursor -> ByteString -> m (Result a)) -> m (Result a)
    {-# INLINE consumeBytestring #-}
    consumeBytestring cursor size k = 
        consume cursor size $ \c@(ByteStringCursor bPtr _ _) addr -> do
            let (Ptr bAddr) = unsafeForeignPtrToPtr bPtr -- эта функция всегда вызывается из withForeignPtr
                off = addr `minusAddr` Addr bAddr
            k c $ fromForeignPtr bPtr off size

{-# INLINABLE runByteString #-}
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
                        