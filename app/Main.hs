{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import qualified Data.ByteString    as BS
import           Data.Int
import           Data.Word
import           Foreign.ForeignPtr
import           Lev.Layout         as L
import           Lev.Monad          as L
import qualified Lev.Reader         as L
import           Prelude            hiding (Monad (..))
import qualified Prelude            as P

read12Int64PlusInt32 :: Reader IO p ('StaticLayout 0 100) Int64
read12Int64PlusInt32 = do
  a1 <- readInt64Host
  a2 <- readInt64Host
  a3 <- readInt64Host
  a4 <- readInt64Host
  a5 <- readInt64Host
  a6 <- readInt64Host
  a7 <- readInt64Host
  a8 <- readInt64Host
  a9 <- readInt64Host
  a10 <- readInt64Host
  a11 <- readInt64Host
  a12 <- readInt64Host
  a13 <- readInt32Host
  return $ a1 + a2 + a3 + a4
         + a5 + a6 + a7 + a8
         + a9 + a10 + a11 + a12
         + fromIntegral a13
{-# INLINE read12Int64PlusInt32 #-}


read4Strings :: Reader IO (ForeignPtr Word8) 'DynamicLayout Int
read4Strings = do
  a1 <- readWord8
  b1 <- readByteString (fromIntegral a1)
  a2 <- readWord8
  b2 <- readByteString (fromIntegral a2)
  a3 <- readWord8
  b3 <- readByteString (fromIntegral a3)
  a4 <- readWord8
  b4 <- readByteString (fromIntegral a4)
  return (BS.length b1 + BS.length b2 + BS.length b3 + BS.length b4)
{-# INLINE read4Strings #-}

testStat :: L.Reader IO (ForeignPtr Word8) 'L.DynamicLayout Int
testStat =
  L.readByteString 666 L.>>= \a ->
  L.return (BS.length a)

main :: IO ()
main = -- do
  -- (a, _) <- L.runReader testDyn $ BS.replicate 999 0
  -- (b, _) <- L.runReader testDyn $ BS.replicate 999 0
  -- print ( a + b + 777 )
  P.return ()
