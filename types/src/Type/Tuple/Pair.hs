{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#include "kinds.h"

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Safe #-}
#endif

module Type.Tuple.Pair
    ( Pair
    , Fst
    , Snd
    )
where

#ifndef DataPolyKinds
-- base ----------------------------------------------------------------------
import           Data.Typeable (Typeable)


-- types ---------------------------------------------------------------------
import {-# SOURCE #-} Type.Bool ((:&&))
import           Type.Eq ((:==))
import           Type.Meta (Known, Val, val, Proxy (Proxy))
import           Type.Ord (Compare)
import           Type.Semigroup ((:<>))


#endif
------------------------------------------------------------------------------
#ifdef DataPolyKinds
#if __GLASGOW_HASKELL__ >= 708
type Pair = '(,)
#else
type Pair a b = '(a, b)
#endif
#else
data Pair a b
  deriving (Typeable)


------------------------------------------------------------------------------
instance (Known a, Known b) => Known (Pair a b) where
    type Val (Pair a b) = (Val a, Val b)
    val _ = (val (Proxy :: Proxy a), val (Proxy :: Proxy b))


------------------------------------------------------------------------------
type instance Pair a b :== Pair a' b' = a :== a' :&& b :== b'


------------------------------------------------------------------------------
type instance Compare (Pair a b) (Pair a' b') = Compare a a' :<> Compare b b'


------------------------------------------------------------------------------
type instance Pair a b :<> Pair a' b' = Pair (a :<> a') (b :<> b')
#endif


------------------------------------------------------------------------------
type family Fst (p :: KPair (KPoly1, KPoly2)) :: KPoly1
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Fst (Pair a _b) = a


------------------------------------------------------------------------------
type family Snd (p :: KPair (KPoly1, KPoly2)) :: KPoly2
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Snd (Pair _a b) = b
