{-# LANGUAGE DataKinds, RankNTypes, ScopedTypeVariables, TypeApplications #-}

module Lev.Reader.Dynamic where

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
import Lev.Reader.Static (Result(..))
import qualified Lev.Reader.Static as Static 

-- Динамические ридеры фактически получаются из статических ридеров, когда состав применяемых статических ридеров
-- определяется читаемыми данным. 

-- TODO: проработать протокол с возвратом точного числа байт, которое нужно ридеру для совершения очередного шага.
-- такой протокол подразумевает возврат к драйверу на каждом шаге (мы не можем просто передать state в k), поэтому 
-- может быть довольно медленным.

-- Можно попробовать изменть направление управления: ридер вызывает драйвер (фунцию diverCallback: ctx -> size -> m (addr, ctx) )

-- TODO: сюда надо передавать k, иначе у драйвера не будет возможности обработать EOS.
type DriverCall m p c a = c -> Int -> (p -> c -> Addr -> m (Result a)) ->  m (Result a)

newtype Reader m p c a = Reader 
    { runReader :: forall r . DriverCall m p c r -> c -> (c -> a -> m (Result r)) -> m (Result r) }

{-# INLINABLE readStatic #-}
readStatic :: forall s m p c a . (KnownNat s) => Static.Reader 0 s m a -> Reader m p c a
readStatic (Static.Reader f) = Reader $ \drv ctx k -> 
    drv ctx (fromIntegral $ natVal $ sing @s) $ \_ ctx' addr -> 
        f addr $ \a -> 
            k ctx' a

runByteString :: Reader IO (ForeignPtr Word8) Addr a -> ByteString -> IO (a, ByteString)
runByteString (Reader f) bs = do
    let (bPtr, bOff, bSize) = toForeignPtr bs
    withForeignPtr bPtr $ \(Ptr bAddr) -> do
        let startAddr = Addr bAddr `plusAddr` bOff
            maxAddr = startAddr `plusAddr` bSize
            drv addr requires k = do
                let nextAddr = addr `plusAddr` requires
                if nextAddr <= maxAddr 
                    then k bPtr nextAddr addr
                    else error "EOS" -- return $ Fail ...
        res <- f drv startAddr (\endAddr a -> return $ Done (a, endAddr))
        case res of 
            Done (a, endAddr) -> return (a, undefined) -- return (a, fromForeignPtr bPtr (bOff + rReq) (bSize - rReq))
            Fail e -> error "fail"

{--
type State p = (p, Addr, Int) -- ^ ( current address, bytes remain in buffer )

data Result m p a = Done !a
                  | Fetch {-# UNPACK #-} !Int (State p -> m (Result m p a))
                  | Fail !SomeException deriving (Typeable)

newtype Reader m p a = Reader 
    { runReader :: forall r . State p -> (State p -> a -> m (Result m p r)) -> m (Result m p r) }

{-# INLINABLE pureReader #-}
pureReader :: a -> Reader m p a
pureReader a = Reader $ \s k -> k s a

{-# INLINABLE bindReader #-}
bindReader :: (a -> Reader p m b) -> Reader p m a -> Reader p m b
bindReader g (Reader f) =  Reader $ \s k -> f s $ \s' a -> runReader (g a) s' k
--}