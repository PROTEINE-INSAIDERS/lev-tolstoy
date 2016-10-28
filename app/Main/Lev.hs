{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax  #-}

module Main.Lev  where

import qualified Data.ByteString as BS
import           Data.Int
import           Lev.Monad       as X
import           Prelude         hiding (Monad (..))

read12Int64PlusInt32 :: Reader IO ('StaticLayout 0 100) Int64
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


read4Strings :: Reader IO 'DynamicLayout Int
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

read4StringsDyn :: Reader IO 'DynamicLayout Int
read4StringsDyn = do
  a1 <- readWord8
  b1 <- readByteString (fromIntegral a1)
  a2 <- readWord8
  b2 <- readByteString (fromIntegral a2)
  a3 <- readWord8
  b3 <- readByteString (fromIntegral a3)
  a4 <- readWord8
  b4 <- readByteString (fromIntegral a4)
  dynRet (BS.length b1 + BS.length b2 + BS.length b3 + BS.length b4)
{-# INLINE read4StringsDyn #-}
