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

module Type.Tuple.Sextet
    ( Sextet
    , Fst
    , Snd
    , Trd
    , Frt
    , Fft
    , Sxt
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
type Sextet = '(,,,,,)
#else
type Sextet a b c d e f = '(a, b, c, d, e, f)
#endif
#else
data Sextet a b c d e f
  deriving (Typeable)


------------------------------------------------------------------------------
instance (Known a, Known b, Known c, Known d, Known e, Known f) =>
    Known (Sextet a b c d e f)
  where
    type Val (Sextet a b c d e f) = (Val a, Val b, Val c, Val d, Val e, Val f)
    val _ = (val (Proxy :: Proxy a), val (Proxy :: Proxy b),
        val (Proxy :: Proxy c), val (Proxy :: Proxy d),
        val (Proxy :: Proxy e), val (Proxy :: Proxy f))


------------------------------------------------------------------------------
type instance Sextet a b c d e f :== Sextet a' b' c' d' e' f' =
    a :== a' :&& b :== b' :&& c :== c' :&& d :== d' :&& e :== e' :&& f :== f'


------------------------------------------------------------------------------
type instance Compare (Sextet a b c d e f) (Sextet a' b' c' d' e' f')
    = Compare a a' :<> Compare b b' :<> Compare c c' :<> Compare d d' :<>
        Compare e e' :<> Compare f f'


------------------------------------------------------------------------------
type instance Sextet a b c d e f :<> Sextet a' b' c' d' e' f'
    = Sextet (a :<> a') (b :<> b') (c :<> c') (d :<> d') (e :<> e') (f :<> f')
#endif


------------------------------------------------------------------------------
type family Fst
    (p :: KSextet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5, KPoly6))
  :: KPoly1
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Fst (Sextet a _b _c _d _e _f) = a


------------------------------------------------------------------------------
type family Snd
    (p :: KSextet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5, KPoly6))
  :: KPoly2
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Snd (Sextet _a b _c _d _e _f) = b


------------------------------------------------------------------------------
type family Trd
    (p :: KSextet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5, KPoly6))
  :: KPoly3
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Trd (Sextet _a _b c _d _e _f) = c


------------------------------------------------------------------------------
type family Frt
    (p :: KSextet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5, KPoly6))
  :: KPoly4
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Frt (Sextet _a _b _c d _e _f) = d


------------------------------------------------------------------------------
type family Fft
    (p :: KSextet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5, KPoly6))
  :: KPoly5
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Fft (Sextet _a _b _c _d e _f) = e


------------------------------------------------------------------------------
type family Sxt
    (p :: KSextet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5, KPoly6))
  :: KPoly6
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Sxt (Sextet _a _b _c _d _e f) = f
