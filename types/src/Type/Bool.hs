{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef DataPolyKinds
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Safe #-}
#endif

#include "kinds.h"

module Type.Bool
    ( False
    , True
    , If
    , Not
    , (:&&)
    , (:||)
    , Add
    , Subtract
    )
where

#ifndef DataPolyKinds
-- base ----------------------------------------------------------------------
import           Data.Typeable (Typeable)


#endif
-- types ---------------------------------------------------------------------
import           Type.Bits
                     ( (:.&.)
                     , (:.|.)
                     , Xor
                     , Complement
                     , RotateL
                     , RotateR
                     , ShiftL
                     , ShiftR
                     )
#ifndef DataPolyKinds
import           Type.Meta (Known, Val, val)
#endif
import           Type.Tuple.Pair (Pair)


#ifdef DataPolyKinds
------------------------------------------------------------------------------
type False = 'False


------------------------------------------------------------------------------
type True = 'True
#else
------------------------------------------------------------------------------
data False
  deriving (Typeable)


------------------------------------------------------------------------------
data True
  deriving (Typeable)


------------------------------------------------------------------------------
instance Known False where
    type Val False = Bool
    val _ = False
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance Known True where
    type Val True = Bool
    val _ = True
    {-# INLINE val #-}
#endif


------------------------------------------------------------------------------
type family If (p :: KBool) (a :: KPoly1) (b :: KPoly1) :: KPoly1
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    If True a _b = a
#ifndef ClosedTypeFamilies
type instance
#endif
    If False _a b = b


------------------------------------------------------------------------------
type family Not (a :: KBool) :: KBool
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    Not False = True
#ifndef ClosedTypeFamilies
type instance
#endif
    Not True  = False


------------------------------------------------------------------------------
type family (a :: KBool) :&& (b :: KBool) :: KBool
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    False :&& _b = False
#ifndef ClosedTypeFamilies
type instance
#endif
    True  :&& b = b
infixr 3 :&&


------------------------------------------------------------------------------
type family (a :: KBool) :|| (b :: KBool) :: KBool
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    True  :|| _b = True
#ifndef ClosedTypeFamilies
type instance
#endif
    False :|| b = b
infixr 2 :||


------------------------------------------------------------------------------
type instance False :.&. False = False
type instance False :.&. True = False
type instance True :.&. False = False
type instance True :.&. True = True


------------------------------------------------------------------------------
type instance False :.|. False = False
type instance False :.|. True = True
type instance True :.|. False = True
type instance True :.|. True = True


------------------------------------------------------------------------------
type instance Xor False False = False
type instance Xor False True = True
type instance Xor True False = True
type instance Xor True True = False


------------------------------------------------------------------------------
type instance Complement False = True
type instance Complement True = False


------------------------------------------------------------------------------
type instance RotateL False = False
type instance RotateL True = True


------------------------------------------------------------------------------
type instance RotateR False = False
type instance RotateR True = True


------------------------------------------------------------------------------
type instance ShiftR False = False
type instance ShiftR True = False


------------------------------------------------------------------------------
type instance ShiftL False = False
type instance ShiftL True = False


------------------------------------------------------------------------------
type family Add (a :: KBool) (b :: KBool) (c :: KBool) :: KPair (KBool, KBool)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    Add False False False = Pair False False
#ifndef ClosedTypeFamilies
type instance
#endif
    Add False False True  = Pair True  False
#ifndef ClosedTypeFamilies
type instance
#endif
    Add False True  False = Pair True  False
#ifndef ClosedTypeFamilies
type instance
#endif
    Add False True  True  = Pair False True
#ifndef ClosedTypeFamilies
type instance
#endif
    Add True  False False = Pair True  False
#ifndef ClosedTypeFamilies
type instance
#endif
    Add True  False True  = Pair False True
#ifndef ClosedTypeFamilies
type instance
#endif
    Add True  True  False = Pair False True
#ifndef ClosedTypeFamilies
type instance
#endif
    Add True  True  True  = Pair True  True


------------------------------------------------------------------------------
type family Subtract (a :: KBool) (b :: KBool) (c :: KBool)
    :: KPair (KBool, KBool)
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    Subtract False False False = Pair False False
#ifndef ClosedTypeFamilies
type instance
#endif
    Subtract False False True = Pair True True
#ifndef ClosedTypeFamilies
type instance
#endif
    Subtract False True False = Pair True True
#ifndef ClosedTypeFamilies
type instance
#endif
    Subtract False True True = Pair False True
#ifndef ClosedTypeFamilies
type instance
#endif
    Subtract True False False = Pair True False
#ifndef ClosedTypeFamilies
type instance
#endif
    Subtract True False True = Pair False False
#ifndef ClosedTypeFamilies
type instance
#endif
    Subtract True True False = Pair False False
#ifndef ClosedTypeFamilies
type instance
#endif
    Subtract True True True = Pair True True
