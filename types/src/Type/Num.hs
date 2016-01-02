{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
#if defined(DataPolyKinds) && __GLASGOW_HASKELL__ >= 706 && __GLASGOW_HASKELL__ < 710
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
#endif

#include "kinds.h"

module Type.Num
    ( (:+)
    , (:-)
    , (:*)
    , (:^)
    )
where

#if __GLASGOW_HASKELL__ >= 706 && defined(DataPolyKinds)
-- base ----------------------------------------------------------------------
import           GHC.TypeLits
                     ( Nat
                     , type (+)
                     , type (*)
                     , type (^)
#if __GLASGOW_HASKELL__ >= 708
                     , type (-)
#endif
                     )


#endif
------------------------------------------------------------------------------
type family (a :: KPoly1) :+ (b :: KPoly1) :: KPoly1
infixl 6 :+


------------------------------------------------------------------------------
type family (a :: KPoly1) :- (b :: KPoly1) :: KPoly1
infixl 6 :-


------------------------------------------------------------------------------
type family (a :: KPoly1) :* (b :: KPoly1) :: KPoly1
infixl 7 :*


------------------------------------------------------------------------------
type family (a :: KPoly1) :^ (b :: KPoly1) :: KPoly1
infixr 8 :^
#if __GLASGOW_HASKELL__ >= 706 && defined(DataPolyKinds)


------------------------------------------------------------------------------
type instance (a :: Nat) :+ (b :: Nat) = a + b


------------------------------------------------------------------------------
type instance (a :: Nat) :* (b :: Nat) = a * b


------------------------------------------------------------------------------
type instance (a :: Nat) :^ (b :: Nat) = a ^ b
#if __GLASGOW_HASKELL__ >= 708


------------------------------------------------------------------------------
type instance (a :: Nat) :- (b :: Nat) = a - b
#endif
#endif