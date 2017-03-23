{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#include "kinds.h"

#ifdef DataPolyKinds
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Safe #-}
#endif

module Type.Bits
    ( (:.&.)
    , (:.|.)
    , Xor
    , Complement
    , RotateL
    , RotateR
    , ShiftL
    , ShiftR
    )
where

------------------------------------------------------------------------------
type family (a :: KPoly1) :.&. (b :: KPoly1) :: KPoly1
infixl 7 :.&.


------------------------------------------------------------------------------
type family (a :: KPoly1) :.|. (b :: KPoly1) :: KPoly1
infixl 5 :.|.


------------------------------------------------------------------------------
type family Xor (a :: KPoly1) (b :: KPoly1) :: KPoly1
infixl 6 `Xor`


------------------------------------------------------------------------------
type family Complement (a :: KPoly1) :: KPoly1


------------------------------------------------------------------------------
type family RotateL (a :: KPoly1) :: KPoly1


------------------------------------------------------------------------------
type family RotateR (a :: KPoly1) :: KPoly1


------------------------------------------------------------------------------
type family ShiftL (a :: KPoly1) :: KPoly1


------------------------------------------------------------------------------
type family ShiftR (a :: KPoly1) :: KPoly1
