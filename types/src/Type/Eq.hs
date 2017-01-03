{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
#if defined(DataPolyKinds) && __GLASGOW_HASKELL__ >= 706 && __GLASGOW_HASKELL__ < 800
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
#endif

#include "kinds.h"

module Type.Eq
    ( (:==)
    , (:/=)
    )
where

#if __GLASGOW_HASKELL__ >= 706 && defined(DataPolyKinds)
-- base ----------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 708
import           Data.Type.Equality (type (==))
#endif
import           GHC.TypeLits
                     ( Nat
#if __GLASGOW_HASKELL__ >= 708
                     , Symbol
#else
                     , type (<=?)
#endif
                     )


#endif
-- types ---------------------------------------------------------------------
import
#ifndef DataPolyKinds
       {-# SOURCE #-}
#endif
                 Type.Bool
                     ( True
                     , False
                     , Not
#ifdef DataPolyKinds
                     , (:&&)
#endif
                     )
import           Type.Ordering (LT, EQ, GT)


------------------------------------------------------------------------------
type family (a :: KPoly1) :== (b :: KPoly1) :: KBool
infix 4 :==


------------------------------------------------------------------------------
type instance False :== False = True
type instance False :== True = False
type instance True :== False = False
type instance True :== True = True


------------------------------------------------------------------------------
type instance LT :== LT = True
type instance LT :== EQ = False
type instance LT :== GT = False
type instance EQ :== LT = False
type instance EQ :== EQ = True
type instance EQ :== GT = False
type instance GT :== LT = False
type instance GT :== EQ = False
type instance GT :== GT = True


#ifdef DataPolyKinds
------------------------------------------------------------------------------
type instance '[] :== '[] = True
type instance '[] :== (_b ': _bs) = False
type instance (_a ': _as) :== '[] = False
type instance (a ': as) :== (b ': bs) = a :== b :&& as :== bs


------------------------------------------------------------------------------
type instance 'Nothing :== 'Nothing = True
type instance 'Nothing :== 'Just _b = False
type instance 'Just _a :== 'Nothing = False
type instance 'Just a :== 'Just b = a :== b


------------------------------------------------------------------------------
type instance 'Left a :== 'Left b = a :== b
type instance 'Left _a :== 'Right _b = False
type instance 'Right _a :== 'Left _b = False
type instance 'Right a :== 'Right b = a :== b


------------------------------------------------------------------------------
type instance '() :== '() = True


------------------------------------------------------------------------------
type instance '(a, b) :== '(a', b') = a :== a' :&& b :== b'


------------------------------------------------------------------------------
type instance '(a, b, c) :== '(a', b', c') =
    a :== a' :&& b :== b' :&& c :== c'


------------------------------------------------------------------------------
type instance '(a, b, c, d) :== '(a', b', c', d') =
    a :== a' :&& b :== b' :&& c :== c' :&& d :== d'


------------------------------------------------------------------------------
type instance '(a, b, c, d, e) :== '(a', b', c', d', e') =
    a :== a' :&& b :== b' :&& c :== c' :&& d :== d' :&& e :== e'


------------------------------------------------------------------------------
type instance '(a, b, c, d, e, f) :== '(a', b', c', d', e', f') =
    a :== a' :&& b :== b' :&& c :== c' :&& d :== d' :&& e :== e' :&& f :== f'


------------------------------------------------------------------------------
type instance '(a, b, c, d, e, f, g) :== '(a', b', c', d', e', f', g') =
    a :== a' :&& b :== b' :&& c :== c' :&& d :== d' :&& e :== e' :&&
        f :== f' :&& g :== g'


#if __GLASGOW_HASKELL__ >= 706
------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ < 708
type instance (a :: Nat) :== (b :: Nat) = a <=? b :&& b <=? a
#else
type instance (a :: Nat) :== (b :: Nat) = a == b


------------------------------------------------------------------------------
type instance (a :: Symbol) :== (b :: Symbol) = a == b
#endif


#endif
#endif
------------------------------------------------------------------------------
type a :/= b = Not (a :== b)
infix 4 :/=
