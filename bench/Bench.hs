module Bench where

import qualified Bench.Binary      as B
import qualified Bench.Handwritten as H
import           Criterion.Main
import           Data.ByteString   as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Int
import qualified Data.Store as ST
import           Data.Word
import qualified Lev.Reader.FixedLength as LS
import qualified Bench.Lev.Reader.Dynamic as LD
import qualified Bench.Lev.Reader.Static as LS
import qualified Lev.Reader.ByteString as LD
import qualified Bench.Store as ST

readerBench :: Benchmark
readerBench = bgroup "reader" [ strict ]
  where
    strict = bgroup "strict"
      [ 
        read1Ginto12Int64plusInt32
      -- , readWord64N16Host
     -- , bigVsLittleEndian
    --  , byteStrings
      ]
      where
        readWord64N16Host =  env setupWord64N16Host $ \ ~buffer ->
          bgroup "Word64N16Host"
          [ bench "store" $ nfIO $ (ST.decodeIOWith $ ST.getWord64N16Host iterations) buffer 
          , bench "store8" $ nfIO $ (ST.decodeIOWith $ ST.getWord8N16 size) buffer 
          , bench "lev" $ nfIO $ (LD.runByteString $ LD.getWord64N16Host iterations) buffer
          , bench "levD" $ nfIO $ (LD.runByteString $ LD.getWord64N16HostD iterations) buffer
          , bench "lev8" $ nfIO $ (LD.runByteString $ LD.getWord8N16 size) buffer
          ]
          where 
            size = 104857600 
            iterations = size `div` 8

            setupWord64N16Host :: IO ByteString
            setupWord64N16Host = return $ BS.replicate size 0

        read1Ginto12Int64plusInt32 = env setup1G $ \ ~buffer ->
          bgroup "read 1G into 12 int64 + int32"
          [  
            bench "Handwritten" $ nf (run H.read12Int64PlusInt32) buffer
          -- , bench "Lev static" $ nfIO $ (runIO $ LS.readByteStringWith LS.read12Int64PlusInt32) buffer
          -- , bench "Lev dynamic" $ nfIO $ (runIO $ LD.runByteString LD.read12Int64PlusInt32) buffer
          , bench "Binary" $ nf (run $ B.runBinaryGetStrict B.read12Int64PlusInt32) buffer
          ]
          where
            buffer1G :: Int
            buffer1G = 100000000 

            {-# INLINE iterations #-}
            iterations :: Int
            iterations = buffer1G `div` 100

            setup1G :: IO ByteString
            setup1G = return $ BS.replicate buffer1G 0

            {-# INLINE run #-}
            run :: (ByteString -> (Int64, ByteString)) -> ByteString -> Int64
            run f = go 0 iterations
              where
                go a 0 _ = a
                go a n s = let (a', s') = f s in go (a + a') (n - 1) s'

            {-# INLINE runIO #-}
            runIO :: (ByteString -> IO (Int64, ByteString)) -> ByteString -> IO Int64
            runIO f = go 0 iterations
              where
                go a 0 _ = return a
                go a n s = do
                  (a', s') <- f s
                  go (a + a') (n - 1) s'

        bigVsLittleEndian = env setupEnv $ \ ~buffer ->
          bgroup "read 1G into 12 int64 + int32"
          [ -- bench "lev big-endian" $ nf bigEndian buffer
          -- , bench "lev little-endian" $ nf littleEndian buffer
          ]
          where
            bufferSize :: Int
            bufferSize = 1000000000
            {-# INLINE bufferSize #-}

            iterations :: Int
            iterations = bufferSize `div` 100
            {-# INLINE iterations #-}

            setupEnv :: IO ByteString
            setupEnv = return $ BS.replicate bufferSize 0

            run :: (ByteString -> (Int64, ByteString)) -> ByteString -> Int64
            run f = go 0 iterations
              where
                go a 0 _ = a
                go a n s = let (a', s') = f s in go (a + a') (n - 1) s'
            {-# INLINE run #-}

        byteStrings = env setupEnv $ \ ~buffer ->
          bgroup "read 100M into 4x25byte strings"
          [ 
            bench "Binary" $ nf binary buffer
          ]
          where
            bufferSize :: Int
            bufferSize = 100000000
            {-# INLINE bufferSize #-}

            iterations :: Int
            iterations = bufferSize `div` 100
            {-# INLINE iterations #-}

            setupEnv :: IO ByteString
            setupEnv = return $ fst $ BS.unfoldrN bufferSize f 0
              where
                f :: Int -> Maybe (Word8, Int)
                f 0 = Just (24, 24)
                f x = Just (0, x - 1)

            run :: (ByteString -> (Int, ByteString)) -> ByteString -> Int
            run f = go 0 iterations
              where
                go a 0 _ = a
                go a n s = let (a', s') = f s in go (a + a') (n - 1) s'
            {-# INLINE run #-}

            {-# INLINE runIO #-}
            runIO :: (ByteString -> IO (Int, ByteString)) -> ByteString -> IO Int
            runIO f = go 0 iterations
              where
                go a 0 _ = return a
                go a n s = do
                  (a', s') <- f s
                  go (a + a') (n - 1) s'

            {-# NOINLINE binary #-}
            binary = run $ B.runBinaryGetStrict B.read4Strings


writerBench :: Benchmark
writerBench = bgroup "Writer" [ strict ]
  where
    strict = bgroup "Strict" [ ]

main :: IO ()
main = defaultMain
  [ readerBench
  , writerBench
  ]
