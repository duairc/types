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

module Type.Tuple.Quartet
    ( Quartet
    , Fst
    , Snd
    , Trd
    , Frt
    )
where

#ifndef DataPolyKinds
-- base ----------------------------------------------------------------------
import           Data.Typeable (Typeable)


-- types ---------------------------------------------------------------------
import           Type.Bool ((:&&))
import           Type.Eq ((:==))
import           Type.Meta (Known, Val, val)
import           Type.Meta.Proxy (Proxy (Proxy))
import           Type.Ord (Compare)
import           Type.Semigroup ((:<>))


#endif
------------------------------------------------------------------------------
#ifdef DataPolyKinds
#if __GLASGOW_HASKELL__ >= 708
type Quartet = '(,,,)
#else
type Quartet a b c d = '(a, b, c, d)
#endif
#else
data Quartet a b c d
  deriving (Typeable)


------------------------------------------------------------------------------
instance (Known a, Known b, Known c, Known d) => Known (Quartet a b c d) where
    type Val (Quartet a b c d) = (Val a, Val b, Val c, Val d)
    val _ = (val (Proxy :: Proxy a), val (Proxy :: Proxy b),
        val (Proxy :: Proxy c), val (Proxy :: Proxy d))


------------------------------------------------------------------------------
type instance Quartet a b c d :== Quartet a' b' c' d' =
    a :== a' :&& b :== b' :&& c :== c' :&& d :== d'


------------------------------------------------------------------------------
type instance Compare (Quartet a b c d) (Quartet a' b' c' d')
    = Compare a a' :<> Compare b b' :<> Compare c c' :<> Compare d d'


------------------------------------------------------------------------------
type instance Quartet a b c d :<> Quartet a' b' c' d'
    = Quartet (a :<> a') (b :<> b') (c :<> c') (d :<> d')
#endif


------------------------------------------------------------------------------
type family Fst (p :: KQuartet (KPoly1, KPoly2, KPoly3, KPoly4)) :: KPoly1
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Fst (Quartet a _b _c _d) = a


------------------------------------------------------------------------------
type family Snd (p :: KQuartet (KPoly1, KPoly2, KPoly3, KPoly4)) :: KPoly2
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Snd (Quartet _a b _c _d) = b


------------------------------------------------------------------------------
type family Trd (p :: KQuartet (KPoly1, KPoly2, KPoly3, KPoly4)) :: KPoly3
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Trd (Quartet _a _b c _d) = c


------------------------------------------------------------------------------
type family Frt (p :: KQuartet (KPoly1, KPoly2, KPoly3, KPoly4)) :: KPoly4
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Frt (Quartet _a _b _c d) = d
