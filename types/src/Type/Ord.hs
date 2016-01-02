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
#if defined(DataPolyKinds) && __GLASGOW_HASKELL__ >= 706 && __GLASGOW_HASKELL__ < 710
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
#endif

#include "kinds.h"

module Type.Ord
    ( Compare
    , (:<)
    , (:>)
    , (:<=)
    , (:>=)
    )
where

#if __GLASGOW_HASKELL__ >= 706 && defined(DataPolyKinds)
-- base ----------------------------------------------------------------------
import           GHC.TypeLits
                     ( Nat
#if __GLASGOW_HASKELL__ >= 708
                     , Symbol
                     , CmpNat
                     , CmpSymbol
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
#if __GLASGOW_HASKELL__ >= 706 && __GLASGOW_HASKELL__ < 708 && defined(DataPolyKinds)
                    , If
#endif
                    )
import           Type.Eq ((:==))
import           Type.Ordering (LT, EQ, GT)
#ifdef DataPolyKinds
import           Type.Semigroup ((:<>))
#endif


------------------------------------------------------------------------------
type family Compare (a :: KPoly1) (b :: KPoly1) :: KOrdering


------------------------------------------------------------------------------
type instance Compare False False = EQ
type instance Compare False True = LT
type instance Compare True False = GT
type instance Compare True True = EQ


------------------------------------------------------------------------------
type instance Compare LT LT = EQ
type instance Compare LT EQ = LT
type instance Compare LT GT = LT
type instance Compare EQ LT = GT
type instance Compare EQ EQ = EQ
type instance Compare EQ GT = LT
type instance Compare GT LT = GT
type instance Compare GT EQ = GT
type instance Compare GT GT = EQ


#ifdef DataPolyKinds
------------------------------------------------------------------------------
type instance Compare '[] '[] = EQ
type instance Compare '[] (_b ': _bs) = LT
type instance Compare (_a ': _as) '[] = GT
type instance Compare (a ': as) (b ': bs) = Compare a b :<> Compare as bs


------------------------------------------------------------------------------
type instance Compare 'Nothing 'Nothing = EQ
type instance Compare 'Nothing ('Just _b) = LT
type instance Compare ('Just _a) 'Nothing = GT
type instance Compare ('Just a) ('Just b) = Compare a b


------------------------------------------------------------------------------
type instance Compare ('Left a) ('Left b) = Compare a b
type instance Compare ('Left _a) ('Right _b) = LT
type instance Compare ('Right _a) ('Left _b) = GT
type instance Compare ('Right a) ('Right b) = Compare a b


------------------------------------------------------------------------------
type instance Compare '() '() = EQ


------------------------------------------------------------------------------
type instance Compare '(a, b) '(a', b') = Compare a a' :<> Compare b b'


------------------------------------------------------------------------------
type instance Compare '(a, b, c) '(a', b', c') =
    Compare a a' :<> Compare b b' :<> Compare c c'


------------------------------------------------------------------------------
type instance Compare '(a, b, c, d) '(a', b', c', d') =
    Compare a a' :<> Compare b b' :<> Compare c c' :<> Compare d d'


------------------------------------------------------------------------------
type instance Compare '(a, b, c, d, e) '(a', b', c', d', e') =
    Compare a a' :<> Compare b b' :<> Compare c c' :<> Compare d d' :<>
        Compare e e'


------------------------------------------------------------------------------
type instance Compare '(a, b, c, d, e, f) '(a', b', c', d', e', f') =
    Compare a a' :<> Compare b b' :<> Compare c c' :<> Compare d d' :<>
        Compare e e' :<> Compare f f'


------------------------------------------------------------------------------
type instance Compare '(a, b, c, d, e, f, g) '(a', b', c', d', e', f', g') =
    Compare a a' :<> Compare b b' :<> Compare c c' :<> Compare d d' :<>
        Compare e e' :<> Compare f f' :<> Compare g g'


#if __GLASGOW_HASKELL__ >= 706
------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ < 708
type instance Compare (a :: Nat) (b :: Nat) =
    If (a <=? b) (If (b <=? a) EQ LT) GT
#else
type instance Compare (a :: Nat) (b :: Nat) = CmpNat a b


------------------------------------------------------------------------------
type instance Compare (a :: Symbol) (b :: Symbol) = CmpSymbol a b
#endif


#endif
#endif
------------------------------------------------------------------------------
type a :< b = Compare a b :== LT
infix 4 :<


------------------------------------------------------------------------------
type a :> b = Compare a b :== GT
infix 4 :>


------------------------------------------------------------------------------
type a :<= b = Not (Compare a b :== GT)
infix 4 :<=


------------------------------------------------------------------------------
type a :>= b = Not (Compare a b :== LT)
infix 4 :>=