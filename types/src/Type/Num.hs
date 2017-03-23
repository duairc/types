{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#include "kinds.h"

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
#if defined(DataPolyKinds) && MIN_VERSION_base(4, 6, 0) && !MIN_VERSION_base(4, 8, 0)
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
#endif

module Type.Num
    ( (:+)
    , (:-)
    , (:*)
    , (:^)
    )
where

#if MIN_VERSION_base(4, 6, 0) && defined(DataPolyKinds)
-- base ----------------------------------------------------------------------
import           GHC.TypeLits
                     ( Nat
                     , type (+)
                     , type (*)
                     , type (^)
#if MIN_VERSION_base(4, 7, 0)
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
#if MIN_VERSION_base(4, 6, 0) && defined(DataPolyKinds)


------------------------------------------------------------------------------
type instance (a :: Nat) :+ (b :: Nat) = a + b


------------------------------------------------------------------------------
type instance (a :: Nat) :* (b :: Nat) = a * b


------------------------------------------------------------------------------
type instance (a :: Nat) :^ (b :: Nat) = a ^ b
#if MIN_VERSION_base(4, 7, 0)


------------------------------------------------------------------------------
type instance (a :: Nat) :- (b :: Nat) = a - b
#endif
#endif
