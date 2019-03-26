{-# LANGUAGE ScopedTypeVariables, DataKinds #-}

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
import Lev.Reader.ByteString
import qualified Lev.Reader.FixedLength as FX

newtype NodeId = NodeId ByteString deriving ( Show )

{-
nodeIdFromString :: String -> NodeId
nodeIdFromString a = NodeId $ pack $ (fromIntegral $ shift len (-8)) : (fromIntegral len) : encoded
    where encoded = encode a 
          len :: Word16 = fromIntegral $ Prelude.length encoded 
-}
nodeIdToString :: NodeId -> String 
nodeIdToString (NodeId nodeId) = toString $ BS.drop 2 nodeId

{-# INLINE readNodeId #-}
readNodeId :: ( ConsumeBytestring c ) => Reader c IO NodeId
readNodeId = do
  size <- fixedLength $ FX.skip (Proxy :: Proxy 1) FX.>>>= \_ -> -- тут лежит кол-во значений.
                        FX.readWord16be 
  liftIO $ Prelude.putStrLn (show size)
  NodeId <$> readByteString (fromIntegral size) 

testKey = do 
  keys <- BS.readFile "/home/schernichkin/Data/FastGraph/R100k/0/values-0000000-0000"
  res <- readWith readNodeId keys 
  return res


-- TODO: как просто определить тайпкласс, означающий, что NodeId может быть прочитана с произвольного байтсринга?


-- testKey = unpack $ nodeIdFromString "testё" -- todo: add length

data OffHeapHashMap = OffHeapHashMap { keys :: ByteString, values :: ByteString }

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

{-- 
class OffHeapHashMap[TKey, TValue](keys: GraphBuffer,
                                   values: GraphBuffer,
                                   hashFunction: Option[HashFunction[TKey]] = None)
                                  (implicit keyDeserializer: Deserializer[TKey],
                                   keySerializer: Serializer[TKey],
                                   valueDeserializer: Deserializer[TValue]) {

  private val numberOfBuckets = keys.size / HashMapUtil.BucketSize

  def size = numberOfBuckets

  def close: Unit = {
    keys.close
    values.close
  }

  def getValue(key: TKey): Option[TValue] = {
    val bucketId = HashMapUtil.computeHash(keySerializer.serialize(key)) % numberOfBuckets
    val keysOffset = bucketId * HashMapUtil.BucketSize

    val keysReader = new BufferReader(keys, keysOffset)
    val valuesOffset = keysReader.readLong()
    val valuesReader = new BufferReader(values, valuesOffset)
    val valuesCount = valuesReader.readByte()

    for (_ <- 0 until valuesCount) {
      val collisionKey = keyDeserializer.deserialize(valuesReader)
      val value = valueDeserializer.deserialize(valuesReader)

      if (collisionKey == key) {
        return Some(value)
      }
    }

    None
  }
}


object Deserializers {

  val IdDeserializer = new Deserializer[String] {
    override def deserialize(reader: BufferReader): String = {
      val size = reader.readShort()
      val bytes = reader.readBytes(size)
      new String(bytes, Serializers.UTF8)
    }
  }

  implicit val LongDeserializer = createDeserializer[Long](_.readLong())

  def createDeserializer[T](read: BufferReader => T) = {
    new Deserializer[T] {
      override def deserialize(reader: BufferReader): T = read(reader)
    }
  }
}

--}