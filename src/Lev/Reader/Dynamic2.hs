{-# LANGUAGE DataKinds, RankNTypes, ScopedTypeVariables, GADTs,
             TypeApplications, UnboxedTuples, PolyKinds  #-}

module Lev.Reader.Dynamic2 where

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

-- задача - избавиться от замыканий и вызовов функции по указателю.
-- 1. функции должны быть верхнеуровневыми и не должны захватывать переменные из лексического окружения. 
-- 2. диспетчирование должно выполняться статически. 

data ByteStringState = ByteStringState !Addr !Addr

data DriverCall s m a where
    ByteStringCall :: ByteStringState -> Int -> (ByteStringState -> Addr -> m (Result a)) -> DriverCall ByteStringState m a

data Result a = Done !a

newtype Reader m s a = Reader 
    { runReader :: forall r . s -> (s -> a -> m (Result r)) -> Result r }

driver :: DriverCall s m a -> m (Result a)
driver (ByteStringCall (ByteStringState addr maxAddr) req k) =
    let nextAddr = addr `plusAddr` req
    in if nextAddr <= maxAddr
        then k (ByteStringState nextAddr maxAddr) addr 
        else error ("EOS: " ++ (show maxAddr) ++ " " ++ (show nextAddr))    

readByteString :: Reader IO ByteStringState a -> ByteString -> IO (a, ByteString)
readByteString (Reader f) bs = do 
    let (bPtr, bOff, bSize) = toForeignPtr bs
    withForeignPtr bPtr $ \(Ptr bAddr) -> do
        let startAddr = Addr bAddr `plusAddr` bOff
            maxAddr = startAddr `plusAddr` bSize
      --  res <- f (ByteStringState addr maxAddr) (\(ByteStringState endAddr _)

        undefined
    undefined
--driver  = 
--    let nextAddr = addr `plusAddr` req
 --   in if nextAddr <= maxAddr
--        then k (ByteStringState nextAddr maxAddr )
--        else error ("EOS: " ++ (show maxAddr) ++ " " ++ (show nextAddr)) -- Failed

-- static :: forall s m a . (KnownNat s) => Static.Reader 0 s m a -> Reader m a
-- static (Static.Reader f) = Reader $ \d k -> undefined  