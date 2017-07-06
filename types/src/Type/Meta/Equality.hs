{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#include "kinds.h"

#ifdef GenericDeriving
{-# LANGUAGE DeriveGeneric #-}
#endif

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
#if MIN_VERSION_base(4, 9, 0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
#endif

module Type.Meta.Equality
    ( (:~:) (Refl)
    , sym, trans, castWith, gcastWith, lift
#ifdef DataPolyKinds
    , apply, inner, outer
#endif
    , TestEquality, testEquality
    )
where

-- base ----------------------------------------------------------------------
#if MIN_VERSION_base(4, 7, 0)
import           Data.Type.Equality
                     ( (:~:) (Refl), TestEquality
                     , sym, trans, castWith, gcastWith
#ifdef DataPolyKinds
                     , apply, inner, outer
#endif
                     , testEquality
                     )
#else
import           Control.Category (Category, id, (.))
#if !defined(DataPolyKinds) || defined(PolyTypeable)
import           Data.Typeable (Typeable)
#endif
import           Prelude hiding ((.), id)
#if defined(DataPolyKinds) && __GLASGOW_HASKELL__ < 708
import           Unsafe.Coerce (unsafeCoerce)
#endif


------------------------------------------------------------------------------
data a :~: b where
    Refl :: a :~: a
#if !defined(DataPolyKinds) || defined(PolyTypeable)
  deriving (Typeable)
#endif
deriving instance Show (a :~: b)
infix 4 :~:


------------------------------------------------------------------------------
instance Eq (a :~: b) where
    Refl == Refl = True


------------------------------------------------------------------------------
instance Ord (a :~: b) where
    compare Refl Refl = EQ


------------------------------------------------------------------------------
instance a ~ b => Enum (a :~: b) where
    toEnum 0 = Refl
    toEnum _ = error "Type.Meta.Equality.toEnum: bad argument"
    fromEnum Refl = 0


------------------------------------------------------------------------------
instance a ~ b => Bounded (a :~: b) where
    minBound = Refl
    maxBound = Refl


------------------------------------------------------------------------------
instance Category (:~:) where
    id = Refl
    Refl . Refl = Refl


------------------------------------------------------------------------------
class TestEquality f where
    testEquality :: f a -> f b -> Maybe (a :~: b)


------------------------------------------------------------------------------
instance TestEquality ((:~:) a) where
    testEquality Refl Refl = Just Refl


------------------------------------------------------------------------------
sym :: (a :~: b) -> (b :~: a)
sym Refl = Refl


------------------------------------------------------------------------------
trans :: (a :~: b) -> (b :~: c) -> (a :~: c)
trans Refl Refl = Refl


------------------------------------------------------------------------------
castWith :: (a :~: b) -> a -> b
castWith Refl x = x


------------------------------------------------------------------------------
gcastWith :: (a :~: b) -> ((a ~ b) => r) -> r
gcastWith Refl x = x
#ifdef DataPolyKinds


------------------------------------------------------------------------------
apply :: (f :~: g) -> (a :~: b) -> (f a :~: g b)
apply Refl Refl = Refl


------------------------------------------------------------------------------
inner :: (f a :~: g b) -> (a :~: b)
inner Refl = f Refl
  where
#if __GLASGOW_HASKELL__ < 708
    f = unsafeCoerce
#else
    f = id
#endif


------------------------------------------------------------------------------
outer :: (f a :~: g b) -> (f :~: g)
outer Refl = f Refl
  where
#if __GLASGOW_HASKELL__ < 708
    f = unsafeCoerce
#else
    f = id
#endif
#endif
#endif


------------------------------------------------------------------------------
lift :: (a :~: b) -> (f a :~: f b)
lift Refl = Refl
