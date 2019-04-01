{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeApplications #-}

module OffHeapHashMap where

import Codec.Binary.UTF8.String
import           Control.Monad.IO.Class
import Data.Bits
import Data.ByteString.UTF8
import Data.ByteString as BS
import Data.Hash.Murmur
import Data.Int
import Data.Proxy
import Data.Word
import Lev.Reader.ByteString as Reader

newtype NodeId = NodeId ByteString deriving (Show, Eq)

{-# INLINE readNodeId #-}
readNodeId :: (Consumable c, Sliceable c) => Reader c IO NodeId
readNodeId = do
   -- это не правильно, потому что в NodeId надо включить её размер. 
   -- нужно добавить комбинатор lookAhead
  size <- fixed readWord16be
  NodeId <$> slice (fromIntegral size)

newtype NodeOffset = NodeOffset Word64 deriving (Show, Eq)

-- сюда, наверное, стоит offset корзинки передать.
{-# INLINE readNodeOffsetFromBucket #-}
readNodeOffsetFromBucket :: (Consumable c, Sliceable c) => NodeId -> Reader c IO (Maybe NodeOffset)
readNodeOffsetFromBucket (NodeId nodeId) = fixed readWord8 >>= go
  where 
    go 0 = return Nothing 
    go n = do
      (NodeId key) <- readNodeId
      if (key == nodeId)
        then Just . NodeOffset <$> fixed readWord64be
        else go $ n - 1

data OffHeapHashMap = OffHeapHashMap !ByteString !ByteString

readNodeOffset :: (Sliceable c) => OffHeapHashMap -> NodeId -> Reader c IO (Maybe NodeOffset)
readNodeOffset (OffHeapHashMap offsets buckets) (NodeId nodeId) = do  
  undefined

{-
nodeIdFromString :: String -> NodeId
nodeIdFromString a = NodeId $ pack $ (fromIntegral $ shift len (-8)) : (fromIntegral len) : encoded
    where encoded = encode a 
          len :: Word16 = fromIntegral $ Prelude.length encoded 
-}

testKey = do 
  keys <- BS.readFile "/home/schernichkin/Data/FastGraph/R100k/0/values-0000000-0000"
  res <- readWith readNodeId keys 
  return res

loadOffHeapHashMap :: String -> String -> IO OffHeapHashMap
loadOffHeapHashMap keys values = OffHeapHashMap <$> (BS.readFile keys) <*> (BS.readFile  values)
    
testMap = loadOffHeapHashMap "/home/schernichkin/Data/FastGraph/R100k/0/keys-0000000-0000" "/home/schernichkin/Data/FastGraph/R100k/0/values-0000000-0000"

{-
getValue :: OffHeapHashMap -> String -> IO (Maybe Word64)
getValue (OffHeapHashMap keys values) key = do
    let keyHash = abs (fromIntegral (murmur3 0x3c074a61 (nodeIdFromString key)):: Int32)
        numberOfBuckets = BS.length keys `div` 8
        bucketId = keyHash `mod` (fromIntegral numberOfBuckets)
        keysOffset = bucketId * 8
    Prelude.putStrLn $ "keyHash = " ++ (show keyHash) ++ " numberOfBuckets =  " ++ (show numberOfBuckets) ++ " bucketId = " ++ (show bucketId) ++ " keysOffset =  " ++ (show keysOffset)
    (valuesOffset, _) <- runByteString (LD.readStatic LS.readWord64le) keys
    Prelude.putStrLn $ "valuesOffset = " ++ (show valuesOffset )
    return Nothing

test = testMap >>= \m -> getValue m "0"
-}
-- val bucketId = HashMapUtil.computeHash(keySerializer.serialize(key)) % numberOfBuckets