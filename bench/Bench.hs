module Bench where

import qualified Bench.Binary      as B
import qualified Bench.Cereal      as C
import qualified Bench.Handwritten as H
import qualified Bench.Lev         as L
import           Criterion.Main
import           Data.ByteString   as BS
import           Data.Int
import           Data.Word

readerBench :: Benchmark
readerBench = bgroup "reader" [ strict ]
  where
    strict = bgroup "strict"
      [ read1Ginto12Int64plusInt32
      , bigVsLittleEndian
      , byteStrings
      ]
      where
        read1Ginto12Int64plusInt32 = env setupEnv $ \ ~buffer ->
          bgroup "read 1G into 12 int64 + int32"
          [
            bench "Handwritten" $ nf handwritten buffer, bench "Lev" $ nfIO $ levReader buffer
          , bench "Binary" $ nf binary buffer
          , bench "Cereal" $ nf cereal buffer
          ]
          where
            {-# INLINE bufferSize #-}
            bufferSize :: Int
            bufferSize = 1000000000

            {-# INLINE iterations #-}
            iterations :: Int
            iterations = bufferSize `div` 100

            setupEnv :: IO ByteString
            setupEnv = return $ BS.replicate bufferSize 0

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

            {-# NOINLINE handwritten #-}
            handwritten = run H.read12Int64PlusInt32

            {-# NOINLINE levReader #-}
            levReader = runIO $ L.runReaderWithByteString L.read12Int64PlusInt32

            {-# NOINLINE binary #-}
            binary = run $ B.runBinaryGetStrict B.read12Int64PlusInt32

            {-# NOINLINE cereal #-}
            cereal = run $ C.runCerealGetStrict C.read12Int64PlusInt32

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
          [ bench "Lev" $ nfIO $ levReader buffer
          , bench "Binary" $ nf binary buffer
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

            {-# NOINLINE levReader #-}
            levReader = runIO $ L.runReaderWithByteString L.read4Strings

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
