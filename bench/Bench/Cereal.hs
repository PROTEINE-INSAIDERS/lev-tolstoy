module Bench.Cereal
  ( runCerealGetStrict
  , read12Int64PlusInt32
  , module X
  ) where

import           Data.ByteString
import           Data.Int
import           Data.Serialize.Get as X

{-# INLINE runCerealGetStrict #-}
runCerealGetStrict :: Get a -> ByteString -> (a, ByteString)
runCerealGetStrict g s = case runGetPartial g s of
  Done a s'  -> (a, s')
  Partial _  -> error   "Bench.Cereal.runCerealGetStrict failed: unexpected Partial."
  Fail msg _ -> error $ "Bench.Cereal.runCerealGetStrict failed with message : " ++ msg

{-# INLINE read12Int64PlusInt32 #-}
read12Int64PlusInt32 :: Get Int64
read12Int64PlusInt32 = do
  a1 <- getWord64host
  a2 <- getWord64host
  a3 <- getWord64host
  a4 <- getWord64host
  a5 <- getWord64host
  a6 <- getWord64host
  a7 <- getWord64host
  a8 <- getWord64host
  a9 <- getWord64host
  a10 <- getWord64host
  a11 <- getWord64host
  a12 <- getWord64host
  a13 <- getWord32host
  return $ (fromIntegral (a1 + a2 + a3 + a4
         + a5 + a6 + a7 + a8
         + a9 + a10 + a11 + a12) :: Int64)
         + fromIntegral a13
