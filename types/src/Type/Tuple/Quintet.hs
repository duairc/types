{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Safe #-}
#endif

#include "kinds.h"

module Type.Tuple.Quintet
    ( Quintet
    , Fst
    , Snd
    , Trd
    , Frt
    , Fft
    )
where

#ifndef DataPolyKinds
-- base ----------------------------------------------------------------------
import           Data.Typeable (Typeable)


#endif
#ifndef DataPolyKinds
-- types ---------------------------------------------------------------------
import           Type.Bool ((:&&))
import           Type.Eq ((:==))
import           Type.Meta (Known, val, Proxy (Proxy))
import           Type.Ord (Compare)
import           Type.Semigroup ((:<>))


#endif
------------------------------------------------------------------------------
#ifdef DataPolyKinds
#if __GLASGOW_HASKELL__ >= 708
type Quintet = '(,,,,)
#else
type Quintet a b c d e = '(a, b, c, d, e)
#endif
#else
data Quintet a b c d e
  deriving (Typeable)


------------------------------------------------------------------------------
instance (Known ra a, Known rb b, Known rc c, Known rd d, Known re e) =>
    Known (ra, rb, rc, rd, re) (Quintet a b c d e)
  where
    val _ = (val (Proxy :: Proxy a), val (Proxy :: Proxy b),
        val (Proxy :: Proxy c), val (Proxy :: Proxy d),
        val (Proxy :: Proxy e))


------------------------------------------------------------------------------
type instance Quintet a b c d e :== Quintet a' b' c' d' e' =
    a :== a' :&& b :== b' :&& c :== c' :&& d :== d' :&& e :== e'


------------------------------------------------------------------------------
type instance Compare (Quintet a b c d e) (Quintet a' b' c' d' e')
    = Compare a a' :<> Compare b b' :<> Compare c c' :<> Compare d d' :<>
        Compare e e'


------------------------------------------------------------------------------
type instance Quintet a b c d e :<> Quintet a' b' c' d' e'
    = Quintet (a :<> a') (b :<> b') (c :<> c') (d :<> d') (e :<> e')
#endif


------------------------------------------------------------------------------
type family Fst (p :: KQuintet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5))
    :: KPoly1
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Fst (Quintet a _b _c _d _e) = a


------------------------------------------------------------------------------
type family Snd (p :: KQuintet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5))
    :: KPoly2
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Snd (Quintet _a b _c _d _e) = b


------------------------------------------------------------------------------
type family Trd (p :: KQuintet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5))
    :: KPoly3
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Trd (Quintet _a _b c _d _e) = c


------------------------------------------------------------------------------
type family Frt (p :: KQuintet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5))
    :: KPoly4
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Frt (Quintet _a _b _c d _e) = d


------------------------------------------------------------------------------
type family Fft (p :: KQuintet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5))
    :: KPoly5
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Fft (Quintet _a _b _c _d e) = e