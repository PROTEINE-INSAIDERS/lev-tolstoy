{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}

module Lev.Reader.Static.Syntax (module Lev.Reader.Static.Syntax, module X) where

import Data.Singletons.Prelude.Num
import Lev.Reader.Static as X 

{-# INLINABLE (>>=) #-}
(>>=) :: ((oa + sa) ~ ob) => Reader oa sa m a -> (a -> Reader ob sb m b) -> Reader oa (sa + sb) m b
(>>=) = (>>>=)

{-# INLINABLE return #-}
return :: a -> Reader o 0 m a
return = pureReader