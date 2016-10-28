{-# LANGUAGE CPP           #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Lev.Layout where

import           Data.Int
import           Data.Singletons
import           Data.Singletons.Prelude.Num
import           Data.Singletons.TypeLits
import           Data.Word
import           GHC.Exts

#include "MachDeps.h"

type family SizeOf a :: Nat

type instance SizeOf Word8 = SIZEOF_WORD8
type instance SizeOf Word16 = SIZEOF_WORD16
type instance SizeOf Word32 = SIZEOF_WORD32
type instance SizeOf Word64 = SIZEOF_WORD64

type instance SizeOf Int8 = SIZEOF_INT8
type instance SizeOf Int16 = SIZEOF_INT16
type instance SizeOf Int32 = SIZEOF_INT32
type instance SizeOf Int64 = SIZEOF_INT64

type Size   = Nat
type Offset = Nat

data Layout = StaticLayout Offset Size
            | DynamicLayout

data instance Sing (a :: Layout) where
  SDynamicLayout :: Sing 'DynamicLayout
  SStaticLayout  :: Sing offset -> Sing size -> Sing ('StaticLayout offset size)

instance (SingI offset, SingI size) => SingI ('StaticLayout offset size) where
  {-# INLINE sing #-}
  sing = SStaticLayout sing sing

instance SingI 'DynamicLayout where
  {-# INLINE sing #-}
  sing = SDynamicLayout

type family BindLayout (a :: Layout) (b :: Layout) :: Layout where
  BindLayout ('StaticLayout o1 s1) ('StaticLayout o2 s2) = 'StaticLayout o1 (s1 :+ s2)
  BindLayout _ _ = 'DynamicLayout

type family BindLayoutInv (a :: Layout) (b :: Layout) :: Constraint where
  BindLayoutInv ('StaticLayout o1 s1) ('StaticLayout o2 s2) = ((o1 :+ s1) ~ o2)
  BindLayoutInv ('StaticLayout o s) 'DynamicLayout = (o ~ 0)
  BindLayoutInv 'DynamicLayout ('StaticLayout o s) = (o ~ 0)
  BindLayoutInv _ _ = ()
