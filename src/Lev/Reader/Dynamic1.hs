{-# LANGUAGE DataKinds, RankNTypes, ScopedTypeVariables, 
             TypeApplications, UnboxedTuples, PolyKinds  #-}

module Lev.Reader.Dynamic1 where

import Control.Exception
import Data.ByteString
import           Data.ByteString.Internal
import Data.Primitive
import           Data.Primitive.Ptr
import           Data.Singletons
import           Data.Singletons.TypeLits
import Data.Typeable
import           Data.Word
import           Foreign.ForeignPtr
-- import Lev.Reader.Static (Result(..))
import qualified Lev.Reader.Static as Static 
import           Control.Monad.ST

data Result m a = Done !a
                | Fail
                | Step !Int !(Addr -> m (Result m a)) 

newtype Reader m a = Reader
    { runReader :: Result m a }

-- тут функтора достаточно.
{-# INLINABLE static #-}
{-# SPECIALISE static :: ( KnownNat s ) => Static.Reader 0 s IO a -> Reader IO a #-}
{-# SPECIALISE static :: ( KnownNat s ) => Static.Reader 0 s (ST st) a -> Reader (ST st) a #-}
static :: forall s m a . ( KnownNat s, Monad m ) => Static.Reader 0 s m a -> Reader m a
static (Static.Reader f) = Reader $ Step (fromIntegral $ natVal $ sing @s) $ \addr -> do
    res <- f addr (return . Static.Done)
    return $ case res of 
        Static.Done a -> Done a
        Static.Fail e -> Fail 

{-# INLINABLE readByteString #-}
readByteString :: Reader IO a -> ByteString -> IO (a, ByteString)
readByteString (Reader f) bs = do
    let (bPtr, bOff, bSize) = toForeignPtr bs
    withForeignPtr bPtr $ \(Ptr bAddr) -> do
        let startAddr = Addr bAddr `plusAddr` bOff
            maxAddr = startAddr `plusAddr` bSize
            
            go res addr = case res of
                Step req g -> do
                    let newAddr = addr `plusAddr` req  
                    if  newAddr <= maxAddr
                        then g newAddr >>= \r ->
                            case r of 
                                Done a -> do
                                   let newOff = addr `minusAddr` Addr bAddr
                                       newSize = bSize - (newOff - bOff)
                                   return (a, fromForeignPtr bPtr newOff newSize)
                                -- go r newAddr
                        else error "EOF"
                Done a -> do 
                    let newOff = addr `minusAddr` Addr bAddr
                        newSize = bSize - (newOff - bOff)
                    return (a, fromForeignPtr bPtr newOff newSize)
                Fail -> error "fail" -- throwIO

        go f startAddr
{-
readByteString :: Reader IO (ForeignPtr Word8) DiverCtx a -> ByteString -> IO (a, ByteString)
readByteString (Reader f) bs = do
    let (bPtr, bOff, bSize) = toForeignPtr bs
    withForeignPtr bPtr $ \(Ptr bAddr) -> do
        let startAddr = Addr bAddr `plusAddr` bOff
            maxAddr = startAddr `plusAddr` bSize            
        res <- f drv (startAddr, maxAddr, bPtr) (\(endAddr, _, _) a -> return $ Done (a, endAddr))
        case res of 
            Done (a, endAddr) -> do
                let newOff = endAddr `minusAddr` Addr bAddr
                    newSize = bSize - (newOff - bOff)
--                Prelude.putStrLn $ "bPtr: " ++ (show bPtr) ++ " bOff: " ++ (show bOff) ++ " bSize: " ++ (show bSize)
--                        ++ " newOff: " ++ (show newOff) ++ " newSize: " ++ (show newSize)
                return (a, fromForeignPtr bPtr newOff newSize) -- return (a, fromForeignPtr bPtr (bOff + rReq) (bSize - rReq))
            Fail e -> throwIO e
    where
        drv (addr, maxAddr, bPtr) requires k = do
            let nextAddr = addr `plusAddr` requires
            if nextAddr <= maxAddr
                then k bPtr (nextAddr, maxAddr, bPtr) addr
                else error ("EOS: " ++ (show maxAddr) ++ " " ++ (show nextAddr)) -- return $ Fail ...
-}