{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

#include "kinds.h"

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
#if MIN_VERSION_base(4, 7, 0) && !MIN_VERSION_base(4, 9, 0)
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
#endif

module Type.Natural
    (
#ifdef DataPolyKinds
      Nat
    ,
#endif
      Natural
    , Zero
    , One
    )
where

-- base ----------------------------------------------------------------------
import           Data.Bits ((.|.), shiftL)
#if MIN_VERSION_base(4, 7, 0) && defined(DataPolyKinds) && \
	!MIN_VERSION_base(4, 11, 0)
import           Data.Type.Equality (type (==))
#endif
import           Data.Typeable (Typeable)
#if MIN_VERSION_base(4, 8, 0)
import qualified Numeric.Natural as N (Natural)
#endif


-- types ---------------------------------------------------------------------
import           Type.Bits ((:.&.), (:.|.), Xor, ShiftL, ShiftR)
import           Type.Eq ((:==))
import           Type.Bool (False, True)
import qualified Type.Bool as B (Add, Subtract)
import           Type.List (Nil, Cons, Reverse)
import           Type.Meta (Known, Val, val)
import           Type.Meta.Proxy (Proxy (Proxy))
import           Type.Num ((:+), (:*), (:^), (:-))
import           Type.Ord (Compare)
import           Type.Ordering (LT, EQ, GT)
import           Type.Semigroup ((:<>))
import           Type.Tuple.Pair (Pair)


------------------------------------------------------------------------------
#ifdef DataPolyKinds
type Natural = 'Natural


------------------------------------------------------------------------------
data Nat = Natural (KList (KBool))
  deriving (Typeable)
#ifdef PolyTypeable
deriving instance Typeable Natural
#endif
#else
data Natural (ns :: KList (KBool))
  deriving (Typeable)
#endif


------------------------------------------------------------------------------
instance Known (Natural Nil) where
    type Val (Natural Nil) =
#if MIN_VERSION_base(4, 8, 0)
        N.Natural
#else
        Integer
#endif
    val _ = 0
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance (Known (Cons a as), Val (Cons a as) ~ [Bool]) =>
    Known (Natural (Cons a as))
  where
    type Val (Natural (Cons a as)) =
#if MIN_VERSION_base(4, 8, 0)
        N.Natural
#else
        Integer
#endif
    val _ = do
        let bits = val (Proxy :: Proxy (Cons a as))
        foldr (\b c -> shiftL c 1 .|. if b then 1 else 0) 0 bits
    {-# INLINE val #-}


------------------------------------------------------------------------------
type Zero = Natural Nil


------------------------------------------------------------------------------
type One = Natural (Cons True Nil)


#if MIN_VERSION_base(4, 7, 0) && defined(DataPolyKinds) &&\
	!MIN_VERSION_base(4, 11, 0)
------------------------------------------------------------------------------
type instance (a :: KNatural) == (b :: KNatural) = a :== b


#endif
------------------------------------------------------------------------------
type instance Natural a :== Natural b = Normalize a :== Normalize b


------------------------------------------------------------------------------
type instance Compare (Natural a) (Natural b) =
    CompareInner (Normalize a) (Normalize b)


------------------------------------------------------------------------------
type instance Natural a :.&. Natural b = Natural (Normalize (And a b))


------------------------------------------------------------------------------
type instance Natural a :.|. Natural b = Natural (Normalize (Or a b))


------------------------------------------------------------------------------
type instance Xor (Natural a) (Natural b) = Natural (Normalize (Xor_ a b))


------------------------------------------------------------------------------
type instance ShiftL (Natural a) = Natural (Cons False a)


------------------------------------------------------------------------------
type instance ShiftR (Natural a) = Natural (Drop a)


------------------------------------------------------------------------------
type instance Natural a :+ Natural b =
    Natural (Add a b Nil (Pair False False))


------------------------------------------------------------------------------
type instance Natural a :- Natural b =
    Natural (Subtract a b Nil (Pair False False))


------------------------------------------------------------------------------
type instance Natural a :* Natural b = Natural (Multiply a b Nil Nil)


------------------------------------------------------------------------------
type instance Natural a :^ Natural b =
    Natural (Normalize (Exp a (Normalize b)))


------------------------------------------------------------------------------
type family Normalize (a :: KList (KBool)) :: KList (KBool)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    Normalize as = Reverse (DropZeroes (Reverse as))


------------------------------------------------------------------------------
type family DropZeroes (xs :: KList (KBool)) :: KList (KBool)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    DropZeroes Nil = Nil
#ifndef ClosedTypeFamilies
type instance
#endif
    DropZeroes (Cons False xs) = DropZeroes xs
#ifndef ClosedTypeFamilies
type instance
#endif
    DropZeroes (Cons True xs) = Cons True xs


------------------------------------------------------------------------------
type family CompareInner (a :: KList (KBool)) (b :: KList (KBool))
    :: KOrdering
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    CompareInner Nil Nil = EQ
#ifndef ClosedTypeFamilies
type instance
#endif
    CompareInner (Cons _a _as) Nil = GT
#ifndef ClosedTypeFamilies
type instance
#endif
    CompareInner Nil (Cons _b _bs) = LT
#ifndef ClosedTypeFamilies
type instance
#endif
    CompareInner (Cons a as) (Cons b bs) = CompareInner as bs :<> Compare a b


------------------------------------------------------------------------------
type family And (a :: KList (KBool)) (b :: KList (KBool)) :: KList (KBool)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    And Nil Nil = Nil
#ifndef ClosedTypeFamilies
type instance
#endif
    And (Cons _a _as) Nil = Nil
#ifndef ClosedTypeFamilies
type instance
#endif
    And Nil (Cons _b _bs) = Nil
#ifndef ClosedTypeFamilies
type instance
#endif
    And (Cons a as) (Cons b bs) = Cons (a :.&. b) (And as bs)


------------------------------------------------------------------------------
type family Or (a :: KList (KBool)) (b :: KList (KBool)) :: KList (KBool)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    Or Nil Nil = Nil
#ifndef ClosedTypeFamilies
type instance
#endif
    Or (Cons a as) Nil = Cons a as
#ifndef ClosedTypeFamilies
type instance
#endif
    Or Nil (Cons b bs) = Cons b bs
#ifndef ClosedTypeFamilies
type instance
#endif
    Or (Cons a as) (Cons b bs) = Cons (a :.|. b) (Or as bs)


------------------------------------------------------------------------------
type family Xor_ (a :: KList (KBool)) (b :: KList (KBool)) :: KList (KBool)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    Xor_ Nil Nil = Nil
#ifndef ClosedTypeFamilies
type instance
#endif
    Xor_ (Cons a as) Nil = Cons a as
#ifndef ClosedTypeFamilies
type instance
#endif
    Xor_ Nil (Cons b bs) = Cons b bs
#ifndef ClosedTypeFamilies
type instance
#endif
    Xor_ (Cons a as) (Cons b bs) = Cons (Xor a b) (Xor_ as bs)


------------------------------------------------------------------------------
type family Drop (xs :: KList (KPoly1)) :: KList (KPoly1)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    Drop Nil = Nil
#ifndef ClosedTypeFamilies
type instance
#endif
    Drop (Cons _x xs) = xs


------------------------------------------------------------------------------
type family Add
    (m :: KList (KBool))
    (n :: KList (KBool))
    (r :: KList (KBool))
    (c :: KPair (KBool, KBool))
  :: KList (KBool)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    Add Nil Nil r (Pair d False) = Drop (Reverse (DropZeroes (Cons d r)))
#ifndef ClosedTypeFamilies
type instance
#endif
    Add Nil Nil r (Pair d True) = Add Nil (Cons True Nil) r (Pair d False)
#ifndef ClosedTypeFamilies
type instance
#endif
    Add (Cons m ms) Nil r (Pair d c) = Add ms Nil (Cons d r) (B.Add m False c)
#ifndef ClosedTypeFamilies
type instance
#endif
    Add Nil (Cons n ns) r (Pair d c) = Add Nil ns (Cons d r) (B.Add False n c)
#ifndef ClosedTypeFamilies
type instance
#endif
    Add (Cons m ms) (Cons n ns) r (Pair d c) =
        Add ms ns (Cons d r) (B.Add m n c)


------------------------------------------------------------------------------
type family Subtract
    (m :: KList (KBool))
    (n :: KList (KBool))
    (r :: KList (KBool))
    (c :: KPair (KBool, KBool))
  :: KList (KBool)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    Subtract Nil Nil r (Pair d False) = Drop (Reverse (DropZeroes (Cons d r)))
#ifndef ClosedTypeFamilies
type instance
#endif
    Subtract (Cons m ms) Nil r (Pair d c) =
        Subtract ms Nil (Cons d r) (B.Subtract m False c)
#ifndef ClosedTypeFamilies
type instance
#endif
    Subtract Nil (Cons n ns) r (Pair d c)
        = Subtract Nil ns (Cons d r) (B.Subtract False n c)
#ifndef ClosedTypeFamilies
type instance
#endif
    Subtract (Cons m ms) (Cons n ns) r (Pair d c) =
        Subtract ms ns (Cons d r) (B.Subtract m n c)


------------------------------------------------------------------------------
type family Multiply
    (m :: KList (KBool))
    (n :: KList (KBool))
    (z :: KList (KBool))
    (r :: KList (KBool))
  :: KList (KBool)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    Multiply _m Nil _z r = Normalize r
#ifndef ClosedTypeFamilies
type instance
#endif
    Multiply m (Cons n ns) z r = Multiply m ns (Cons False z)
        (Add r (MultiplyStep m n z) Nil (Pair False False))


------------------------------------------------------------------------------
type family MultiplyStep
    (m :: KList (KBool))
    (n :: KBool)
    (r :: KList (KBool))
  :: KList (KBool)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    MultiplyStep Nil _n r = Reverse (DropZeroes r)
#ifndef ClosedTypeFamilies
type instance
#endif
    MultiplyStep (Cons m ms) n r = MultiplyStep ms n (Cons (m :.&. n) r)


------------------------------------------------------------------------------
type family Exp (a :: KList (Bool)) (b :: KList (Bool)) :: KList (Bool)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    Exp _x Nil = Cons True Nil
#ifndef ClosedTypeFamilies
type instance
#endif
    Exp x (Cons False ns) = Exp (Multiply x x Nil Nil) ns
#ifndef ClosedTypeFamilies
type instance
#endif
    Exp x (Cons True ns) = Multiply x (Exp (Multiply x x Nil Nil) ns) Nil Nil
