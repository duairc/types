{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
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

module Type.List
    ( Cons
    , Nil
    , Reverse
    )
where

#ifndef DataPolyKinds
-- base ----------------------------------------------------------------------
import           Data.Typeable (Typeable)


-- types ---------------------------------------------------------------------
import           Type.Bool (True, False, (:&&))
import           Type.Eq ((:==))
import           Type.Meta (Known, val, Proxy (Proxy), Void)
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
instance Known [Void] Nil where
    val _ = []


------------------------------------------------------------------------------
data Cons x xs
  deriving (Typeable)
infixr 5 `Cons`


------------------------------------------------------------------------------
instance Known r a => Known [r] (Cons a Nil) where
    val _ = [val (Proxy :: Proxy a)]


------------------------------------------------------------------------------
instance
#ifdef OverlapPragma
    {-# OVERLAPS #-}
#endif
    (Known r a, Known [r] as) => Known [r] (Cons a as)
  where
    val _ = val (Proxy :: Proxy a) : val (Proxy :: Proxy as)


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