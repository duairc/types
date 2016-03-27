{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

#include "kinds.h"

#if defined(DataPolyKinds)
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#if defined(SafeHaskell)
#if __GLASGOW_HASKELL__ < 704 || __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
#endif

module Data.Uncurry
    ( Uncurry (Uncurry)
    )
where

-- base ----------------------------------------------------------------------
import           Data.Ix (Ix, range, index, inRange)
import           Data.Monoid (Monoid, mappend, mempty)
#if MIN_VERSION_base(4, 9, 0)
import           Data.Semigroup (Semigroup, (<>))
#endif
#if defined(PolyTypeable)
import           Data.Typeable (Typeable)
#endif
import           Foreign.Ptr (castPtr)
import           Foreign.Storable (Storable, alignment, peek, poke, sizeOf)
#if MIN_VERSION_base(4, 4, 0)
import           GHC.Generics
                     ( D1
                     , C1
                     , Generic
                     , K1 (K1)
                     , M1 (M1)
                     , S1
                     , Rec0
                     , Rep
#if __GLASGOW_HASKELL__ >= 800
                     , FixityI (PrefixI)
                     , Meta (MetaCons, MetaData, MetaSel)
                     , SourceUnpackedness (NoSourceUnpackedness)
                     , SourceStrictness (SourceStrict)
                     , DecidedStrictness (DecidedStrict)
#else
                     , Constructor
                     , Datatype
                     , NoSelector
                     , conName
                     , datatypeName
                     , moduleName
#endif
                     , from
                     , to
                     )
#endif


-- types ---------------------------------------------------------------------
import           Type.Tuple.Pair (Pair)


------------------------------------------------------------------------------
data Uncurry f (p :: KPair (KPoly1, KPoly2)) where
    -- This is strict because Uncurry is morally a newtype
    Uncurry :: !(f a b) -> Uncurry f (Pair a b)
#if defined(PolyTypeable)
  deriving (Typeable)
#endif
deriving instance (Read (f a b)) => Read (Uncurry f (Pair a b))
deriving instance (Show (f a b)) => Show (Uncurry f (Pair a b))


------------------------------------------------------------------------------
instance Eq (f a b) => Eq (Uncurry f (Pair a b)) where
    Uncurry a == Uncurry b = a == b


------------------------------------------------------------------------------
instance Ord (f a b) => Ord (Uncurry f (Pair a b)) where
    compare (Uncurry a) (Uncurry b) = compare a b


------------------------------------------------------------------------------
instance Bounded (f a b) => Bounded (Uncurry f (Pair a b)) where
    minBound = Uncurry minBound
    maxBound = Uncurry maxBound


------------------------------------------------------------------------------
instance Enum (f a b) => Enum (Uncurry f (Pair a b)) where
    toEnum = Uncurry . toEnum
    fromEnum (Uncurry f) = fromEnum f


------------------------------------------------------------------------------
instance Ix (f a b) => Ix (Uncurry f (Pair a b)) where
    range (Uncurry a, Uncurry b) = map Uncurry (range (a, b))
    index (Uncurry a, Uncurry b) (Uncurry i) = index (a, b) i
    inRange (Uncurry a, Uncurry b) (Uncurry i) = inRange (a, b) i


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance Semigroup (f a b) => Semigroup (Uncurry f (Pair a b)) where
    Uncurry a <> Uncurry b = Uncurry (a <> b)


#endif
------------------------------------------------------------------------------
instance Monoid (f a b) => Monoid (Uncurry f (Pair a b)) where
    mempty = Uncurry mempty
    mappend (Uncurry a) (Uncurry b) = Uncurry (mappend a b)


------------------------------------------------------------------------------
instance Storable (f a b) => Storable (Uncurry f (Pair a b)) where
    sizeOf (_ :: Uncurry f (Pair a b)) = sizeOf (undefined :: f a b)
    alignment (_ :: Uncurry f (Pair a b)) = alignment (undefined :: f a b)
    peek = fmap Uncurry . peek . castPtr
    poke ptr (Uncurry a) = poke (castPtr ptr) a


#if MIN_VERSION_base(4, 4, 0)
------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 711
type UncurryMetaData
    = 'MetaData "Uncurry" "Data.Bifunctor.Uncurry" "main" 'False
type UncurryMetaCons = 'MetaCons "Uncurry" 'PrefixI 'False
type UncurryMetaSel
    = 'MetaSel 'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict
#else
data UncurryMetaData
data UncurryMetaCons
type UncurryMetaSel = NoSelector


------------------------------------------------------------------------------
instance Datatype UncurryMetaData where
    datatypeName _ = "Uncurry"
    moduleName _ = "Data.Uncurry"


------------------------------------------------------------------------------
instance Constructor UncurryMetaCons where
    conName _ = "Uncurry"
#endif


------------------------------------------------------------------------------
instance Generic (Uncurry f (Pair a b)) where
    type Rep (Uncurry f (Pair a b)) = D1 UncurryMetaData
        (C1 UncurryMetaCons (S1 UncurryMetaSel (Rec0 (f a b))))
    from (Uncurry f) = M1 (M1 (M1 (K1 f)))
    to (M1 (M1 (M1 (K1 f)))) = Uncurry f


#endif