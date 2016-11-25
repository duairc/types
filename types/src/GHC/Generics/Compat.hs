{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

#include "kinds.h"

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE ExplicitNamespaces #-}
#endif

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

module GHC.Generics.Compat
    ( V1
    , U1 (U1)
    , Par1 (Par1, unPar1)
    , Rec1 (Rec1, unRec1)
    , K1 (K1, unK1)
    , M1 (M1, unM1)
    , (:+:) (L1, R1)
    , (:*:) ((:*:))
    , (:.:) (Comp1, unComp1)

#if __GLASGOW_HASKELL__ >= 800
    , URec
        ( UAddr
        , UChar
        , UDouble
        , UFloat
        , UInt
        , UWord
        , uAddr#
        , uChar#
        , uDouble#
        , uFloat#
        , uInt#
        , uWord#
        )
    , UAddr
    , UChar
    , UDouble
    , UFloat
    , UInt
    , UWord

#endif
    , Rec0
    , R
    , D1
    , C1
    , S1
    , D
    , C
    , S

    , Datatype (..)
    , Constructor (..)
    , Selector (..)
    , Fixity (Prefix, Infix)
    , FixityI (PrefixI, InfixI)
    , PrefixI
    , InfixI
    , Associativity (LeftAssociative, RightAssociative, NotAssociative)
    , LeftAssociative
    , RightAssociative
    , NotAssociative
    , prec
    , Meta (MetaData, MetaCons, MetaSel)
    , MetaData
    , MetaCons
    , MetaSel
    , DecidedStrictness (DecidedLazy, DecidedStrict, DecidedUnpack)
    , DecidedLazy
    , DecidedStrict
    , DecidedUnpack
    , SourceStrictness (NoSourceStrictness, SourceLazy, SourceStrict)
    , NoSourceStrictness
    , SourceLazy
    , SourceStrict
    , SourceUnpackedness (NoSourceUnpackedness, SourceNoUnpack, SourceUnpack)
    , NoSourceUnpackedness
    , SourceNoUnpack
    , SourceUnpack

    , Generic (type Rep, from, to)
    , Generic1 (type Rep1, from1, to1)
    )
where

-- base ----------------------------------------------------------------------
#if __GLASGOW_HASKELL__ < 800
import          Data.Ix (Ix)
#endif
import          GHC.Generics
#if __GLASGOW_HASKELL__ >= 710
import          Numeric.Natural (Natural)
#endif


-- types ---------------------------------------------------------------------
#if defined(DataPolyKinds) && __GLASGOW_HASKELL__ < 800
import          GHC.TypeLits.Compat (Nat, Symbol)
#endif
#if __GLASGOW_HASKELL__ < 800
import          Type.Maybe (Just, Nothing)
#endif
import          Type.Meta (Known, Proxy (Proxy), val)


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 710
type NatVal = Natural
#else
type NatVal = Integer
#endif
#ifndef DataPolyKinds


------------------------------------------------------------------------------
type Nat = NatVal
type Symbol = String
#endif
#if __GLASGOW_HASKELL__ < 800


------------------------------------------------------------------------------
data FixityI = PrefixI | InfixI Associativity Nat


------------------------------------------------------------------------------
data Meta
    = MetaData Symbol Symbol Symbol Bool
    | MetaCons Symbol FixityI Bool
    | MetaSel
        (Maybe Symbol)
        SourceUnpackedness
        SourceStrictness
        DecidedStrictness


------------------------------------------------------------------------------
data DecidedStrictness
    = DecidedLazy
    | DecidedStrict
    | DecidedUnpack
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Ix, Generic)


------------------------------------------------------------------------------
data SourceStrictness
    = NoSourceStrictness
    | SourceStrict
    | SourceLazy
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Ix, Generic)


------------------------------------------------------------------------------
data SourceUnpackedness
    = NoSourceUnpackedness
    | SourceNoUnpack
    | SourceUnpack
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Ix, Generic)
#endif


------------------------------------------------------------------------------
#ifdef DataPolyKinds
type PrefixI = 'PrefixI
type InfixI = 'InfixI
type LeftAssociative = 'LeftAssociative
type RightAssociative = 'RightAssociative
type NotAssociative = 'NotAssociative
#if __GLASGOW_HASKELL__ >= 800
type MetaData = 'MetaData
type MetaCons = 'MetaCons
type MetaSel = 'MetaSel
#else
-- D1 etc will want a *-kinded type argument on GHC < 8; use Proxy to get this
type MetaData n m p nt = Proxy ('MetaData n m p nt)
type MetaCons n f s = Proxy ('MetaCons n f s)
type MetaSel mn su ss ds = Proxy ('MetaSel mn su ss ds)
#endif
type DecidedLazy = 'DecidedLazy
type DecidedStrict = 'DecidedStrict
type DecidedUnpack = 'DecidedUnpack
type NoSourceStrictness = 'NoSourceStrictness
type SourceLazy = 'SourceLazy
type SourceStrict = 'SourceStrict
type NoSourceUnpackedness = 'NoSourceUnpackedness
type SourceNoUnpack = 'SourceNoUnpack
type SourceUnpack = 'SourceUnpack
#else
data PrefixI
data InfixI a n
data LeftAssociative
data RightAssociative
data NotAssociative
data MetaData n m p nt
data MetaCons n f s
data MetaSel mn su ss ds
data DecidedLazy
data DecidedStrict
data DecidedUnpack
data NoSourceStrictness
data SourceLazy
data SourceStrict
data NoSourceUnpackedness
data SourceNoUnpack
data SourceUnpack
#endif


------------------------------------------------------------------------------
instance (Known Fixity PrefixI) where val _ = Prefix
instance (Known Associativity a, Known NatVal n) =>
    Known Fixity (InfixI a n)
  where
    val _ =
        Infix (val (Proxy :: Proxy a)) (fromIntegral (val (Proxy :: Proxy n)))
instance Known Associativity LeftAssociative where val _ = LeftAssociative
instance Known Associativity RightAssociative where val _ = RightAssociative
instance Known Associativity NotAssociative where val _ = NotAssociative
instance Known DecidedStrictness DecidedLazy where val _ = DecidedLazy
instance Known DecidedStrictness DecidedStrict where val _ = DecidedStrict
instance Known DecidedStrictness DecidedUnpack where val _ = DecidedUnpack
instance Known SourceStrictness NoSourceStrictness where
    val _ = NoSourceStrictness
instance Known SourceStrictness SourceLazy where val _ = SourceLazy
instance Known SourceStrictness SourceStrict where val _ = SourceStrict
instance Known SourceUnpackedness NoSourceUnpackedness where
    val _ = NoSourceUnpackedness
instance Known SourceUnpackedness SourceNoUnpack where val _ = SourceNoUnpack
instance Known SourceUnpackedness SourceUnpack where val _ = SourceUnpack
#if __GLASGOW_HASKELL__ < 800


------------------------------------------------------------------------------
instance (Known String n, Known String m, Known Bool nt) =>
    Datatype (MetaData n m p nt)
  where
    datatypeName _ = val (Proxy :: Proxy n)
    moduleName _ = val (Proxy :: Proxy m)
#if __GLASGOW_HASKELL__ >= 708
    isNewtype _ = val (Proxy :: Proxy nt)
#endif


------------------------------------------------------------------------------
instance (Known String n, Known Fixity f, Known Bool r) =>
    Constructor (MetaCons n f r)
  where
    conName _ = val (Proxy :: Proxy n)
    conFixity _ = val (Proxy :: Proxy f)
    conIsRecord _ = val (Proxy :: Proxy r)


------------------------------------------------------------------------------
instance Known String s => Selector (MetaSel (Just s) su ss ds) where
    selName _ = val (Proxy :: Proxy s)


------------------------------------------------------------------------------
instance Selector (MetaSel Nothing su ss ds) where
    selName _ = ""
#endif
