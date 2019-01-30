{-# LANGUAGE RankNTypes #-}

module Lev.Reader.Dynamic where

import Control.Exception
import Data.ByteString
import Data.Primitive
import Data.Typeable
import qualified Lev.Reader.Static as Static 

type State = (Addr, Int) -- ^ ( current address, bytes read )

data Result m p a = Done !a
                  | Fetch {-# UNPACK #-} !Int (p -> State -> m (Result m p a))
                  | Fail !SomeException deriving (Typeable)

newtype Reader m p a = Reader 
    { runReader :: forall r . p -> State -> (State -> a -> m (Result m p r)) -> m (Result m p r) }

{-# INLINABLE pureReader #-}
pureReader :: a -> Reader m p a
pureReader a = Reader $ \_ s k -> k s a

{-# INLINABLE bindReader #-}
bindReader :: (a -> Reader p m b) -> Reader p m a -> Reader p m b
bindReader g (Reader f) =  Reader $ \p s k -> f p s $ \s' a -> runReader (g a) p s' k
                                                                            -- ^ это не правильно, т.к. f в процессе работы может вызвать Fetch,
                                                                            --   тогда в k потребуется передать новое значение p. 
