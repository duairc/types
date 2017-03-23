{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#include "kinds.h"

#ifdef DataPolyKinds
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Safe #-}
#endif

module Type.Bool
    ( False
    , True
    , If
    , Not
    , (:&&)
    , (:||)
    )
where

#ifdef DataPolyKinds
------------------------------------------------------------------------------
type False = 'False


------------------------------------------------------------------------------
type True = 'True
#else
------------------------------------------------------------------------------
data False


------------------------------------------------------------------------------
data True
#endif


------------------------------------------------------------------------------
type family If (p :: KBool) (a :: KPoly1) (b :: KPoly1) :: KPoly1
#ifdef ClosedTypeFamilies
  where
    If True a _b = a
    If False _a b = b
#endif


------------------------------------------------------------------------------
type family Not (a :: KBool) :: KBool
#ifdef ClosedTypeFamilies
  where
    Not False = True
    Not True  = False
#endif


------------------------------------------------------------------------------
type family (a :: KBool) :&& (b :: KBool) :: KBool
#ifdef ClosedTypeFamilies
  where
    False :&& _b = False
    True  :&& b = b
#endif
infixr 3 :&&


------------------------------------------------------------------------------
type family (a :: KBool) :|| (b :: KBool) :: KBool
#ifdef ClosedTypeFamilies
  where
    True  :|| _b = True
    False :|| b = b
#endif
