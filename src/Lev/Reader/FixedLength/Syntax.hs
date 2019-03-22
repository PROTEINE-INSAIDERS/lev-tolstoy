{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}

module Lev.Reader.FixedLength.Syntax (module Lev.Reader.FixedLength.Syntax, module X) where

import Data.Singletons.Prelude.Num
import Lev.Reader.FixedLength as X 

{-# INLINE (>>=) #-}
(>>=) :: ((oa + sa) ~ ob) => Reader oa sa m a -> (a -> Reader ob sb m b) -> Reader oa (sa + sb) m b
(>>=) = (>>>=)

{-# INLINE return #-}
return :: a -> Reader o 0 m a
return = pureReader