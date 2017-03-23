{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Type.List
    ( Cons
    , Nil
    , Head
    , Tail
    , Reverse
    , Snoc
    , Init
    , Last
    )
where

#ifndef DataPolyKinds
-- base ----------------------------------------------------------------------
import           Data.Typeable (Typeable)


-- types ---------------------------------------------------------------------
import           Type.Bool (True, False, (:&&))
import           Type.Eq ((:==))
import           Type.Meta (Known, Val, val, Proxy (Proxy), Void)
import           Type.Ord (Compare)
import           Type.Ordering (LT, EQ, GT)
import           Type.Semigroup ((:<>))


#endif
#ifdef DataPolyKinds
------------------------------------------------------------------------------
type Nil = '[]


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 708
type Cons = '(:)
#else
type Cons a b = (a ': b)
#endif
#else
------------------------------------------------------------------------------
data Nil
  deriving (Typeable)


------------------------------------------------------------------------------
instance Known Nil where
    type Val Nil = [Void]
    val _ = []


------------------------------------------------------------------------------
data Cons x xs
  deriving (Typeable)
infixr 5 `Cons`


------------------------------------------------------------------------------
instance Known a => Known (Cons a Nil) where
    type Val (Cons a Nil) = [Val a]
    val _ = [val (Proxy :: Proxy a)]


------------------------------------------------------------------------------
instance (Known a, Known (Cons a' as), Val (Cons a' as) ~ [Val a]) =>
    Known (Cons a (Cons a' as))
  where
    type Val (Cons a (Cons a' as)) = [Val a]
    val _ = val (Proxy :: Proxy a) : val (Proxy :: Proxy (Cons a' as))


------------------------------------------------------------------------------
type instance Nil :== Nil = True
type instance Nil :== Cons _b _bs = False
type instance Cons _a _as :== Nil = False
type instance Cons a as :== Cons b bs = a :== b :&& as :== bs


------------------------------------------------------------------------------
type instance Compare Nil Nil = EQ
type instance Compare Nil (Cons _b _bs) = LT
type instance Compare (Cons _a _as) Nil = GT
type instance Compare (Cons a as) (Cons b bs) = Compare a b :<> Compare b bs


------------------------------------------------------------------------------
type instance Nil :<> Nil = Nil
type instance Cons a as :<> Nil = Cons a as
type instance Nil :<> Cons b bs = Cons b bs
type instance Cons a as :<> Cons b bs = Cons a (as :<> Cons b bs)
#endif


------------------------------------------------------------------------------
type family Head (as :: KList (KPoly1)) :: KPoly1
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Head (Cons a _as) = a


------------------------------------------------------------------------------
type family Tail (as :: KList (KPoly1)) :: KList (KPoly1)
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Tail (Cons _a as) = as


------------------------------------------------------------------------------
type family Reverse (as :: KList (KPoly1)) :: KList (KPoly1)
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    Reverse as = ReverseInner as Nil


------------------------------------------------------------------------------
type family ReverseInner (xs :: KList (KPoly1)) (a :: KList (KPoly1))
    :: KList (KPoly1)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    ReverseInner Nil a = a
#ifndef ClosedTypeFamilies
type instance
#endif
    ReverseInner (Cons x xs) a = ReverseInner xs (Cons x a)


------------------------------------------------------------------------------
type family Snoc (as :: KList (KPoly1)) (a :: KPoly1) :: KList (KPoly1)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    Snoc Nil b = Cons b Nil
#ifndef ClosedTypeFamilies
type instance
#endif
    Snoc (Cons a as) b = Cons a (Snoc as b)


------------------------------------------------------------------------------
type family Init (as :: KList (KPoly1)) :: KList (KPoly1)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    Init (Cons a Nil) = Nil
#ifndef ClosedTypeFamilies
type instance
#endif
    Init (Cons a (Cons a' as)) = Cons a (Init (Cons a' as))


------------------------------------------------------------------------------
type family Last (as :: KList (KPoly1)) :: KPoly1
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    Last (Cons a Nil) = a
#ifndef ClosedTypeFamilies
type instance
#endif
    Last (Cons _a (Cons a as)) = Last (Cons a as)
