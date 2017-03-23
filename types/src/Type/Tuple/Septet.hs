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

module Type.Tuple.Septet
    ( Septet
    , Fst
    , Snd
    , Trd
    , Frt
    , Fft
    , Sxt
    , Svnt
    )
where

#ifndef DataPolyKinds
-- base ----------------------------------------------------------------------
import           Data.Typeable (Typeable)


-- types ---------------------------------------------------------------------
import           Type.Bool ((:&&))
import           Type.Eq ((:==))
import           Type.Meta (Known, Val, val, Proxy (Proxy))
import           Type.Ord (Compare)
import           Type.Semigroup ((:<>))


#endif
------------------------------------------------------------------------------
#ifdef DataPolyKinds
#if __GLASGOW_HASKELL__ >= 708
type Septet = '(,,,,,,)
#else
type Septet a b c d e f g = '(a, b, c, d, e, f, g)
#endif
#else
data Septet a b c d e f g
  deriving (Typeable)


------------------------------------------------------------------------------
instance (Known a, Known b, Known c, Known d, Known e, Known f, Known g) =>
    Known (Septet a b c d e f g)
  where
    type Val (Septet a b c d e f g) =
        (Val a, Val b, Val c, Val d, Val e, Val f, Val g)
    val _ = (val (Proxy :: Proxy a), val (Proxy :: Proxy b),
        val (Proxy :: Proxy c), val (Proxy :: Proxy d),
        val (Proxy :: Proxy e), val (Proxy :: Proxy f),
        val (Proxy :: Proxy g))


------------------------------------------------------------------------------
type instance Septet a b c d e f g :== Septet a' b' c' d' e' f' g' =
    a :== a' :&& b :== b' :&& c :== c' :&& d :== d' :&& e :== e' :&&
        f :== f' :&& g :== g'


------------------------------------------------------------------------------
type instance Compare (Septet a b c d e f g) (Septet a' b' c' d' e' f' g')
    = Compare a a' :<> Compare b b' :<> Compare c c' :<> Compare d d' :<>
        Compare e e' :<> Compare f f' :<> Compare g g'


------------------------------------------------------------------------------
type instance Septet a b c d e f g :<> Septet a' b' c' d' e' f' g'
    = Septet (a :<> a') (b :<> b') (c :<> c') (d :<> d') (e :<> e') (f :<> f')
        (g :<> g')
#endif


------------------------------------------------------------------------------
type family Fst
    (p :: KSeptet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5, KPoly6, KPoly7))
  :: KPoly1
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Fst (Septet a _b _c _d _e _f _g) = a


------------------------------------------------------------------------------
type family Snd
    (p :: KSeptet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5, KPoly6, KPoly7))
  :: KPoly2
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Snd (Septet _a b _c _d _e _f _g) = b


------------------------------------------------------------------------------
type family Trd
    (p :: KSeptet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5, KPoly6, KPoly7))
  :: KPoly3
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Trd (Septet _a _b c _d _e _f _g) = c


------------------------------------------------------------------------------
type family Frt
    (p :: KSeptet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5, KPoly6, KPoly7))
  :: KPoly4
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Frt (Septet _a _b _c d _e _f _g) = d


------------------------------------------------------------------------------
type family Fft
    (p :: KSeptet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5, KPoly6, KPoly7))
  :: KPoly5
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Fft (Septet _a _b _c _d e _f _g) = e


------------------------------------------------------------------------------
type family Sxt
    (p :: KSeptet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5, KPoly6, KPoly7))
  :: KPoly6
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Sxt (Septet _a _b _c _d _e f _g) = f


------------------------------------------------------------------------------
type family Svnt
    (p :: KSeptet (KPoly1, KPoly2, KPoly3, KPoly4, KPoly5, KPoly6, KPoly7))
  :: KPoly7
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Svnt (Septet _a _b _c _d _e _f g) = g
