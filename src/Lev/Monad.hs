{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module Lev.Monad
  ( LevMonad (..)
  , module X
  ) where

import           Data.Singletons
import           Lev.Layout      as X
import           Lev.Reader      as X
import           Prelude         (Monad (), String, error )

class LevMonad (m :: Layout -> * -> *) where
  (>>=)  :: (SingI f, SingI g) => (BindLayoutInv f g) => m f a -> (a -> m g b) -> m (BindLayout f g) b
  return :: a -> m ('StaticLayout o 0) a
  fail   :: String -> m l a
  fail = error

instance (Monad m) => LevMonad (Reader m p) where
  (>>=) = bindReader
  {-# INLINE (>>=) #-}
  return = unitReader
  {-# INLINE return #-}

