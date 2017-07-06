{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE ExplicitNamespaces #-}
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

#if MIN_VERSION_base(4, 9, 0)
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
#if !MIN_VERSION_base(4, 9, 0)
import          Data.Ix (Ix)
#endif
import          GHC.Generics
#if MIN_VERSION_base(4, 8, 0)
import          Numeric.Natural (Natural)
#endif


-- types ---------------------------------------------------------------------
#if defined(DataPolyKinds) && !MIN_VERSION_base(4, 9, 0)
import          GHC.TypeLits.Compat (Nat, Symbol)
#endif
#if !MIN_VERSION_base(4, 9, 0)
import          Type.Maybe (Just, Nothing)
#endif
import           Type.Meta (Known, Val, val)
import           Type.Meta.Proxy (Proxy (Proxy))


------------------------------------------------------------------------------
#if MIN_VERSION_base(4, 8, 0)
type NatVal = Natural
#else
type NatVal = Integer
#endif
#ifndef DataPolyKinds


------------------------------------------------------------------------------
type Nat = NatVal
type Symbol = String
#endif
#if !MIN_VERSION_base(4, 9, 0)


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
#if MIN_VERSION_base(4, 9, 0)
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
instance (Known PrefixI) where
    type Val PrefixI = Fixity
    val _ = Prefix
instance (Known a, Val a ~ Associativity, Known n, Val n ~ NatVal) =>
    Known (InfixI a n)
  where
    type Val (InfixI a n) = Fixity
    val _ =
        Infix (val (Proxy :: Proxy a)) (fromIntegral (val (Proxy :: Proxy n)))
instance Known LeftAssociative where
    type Val LeftAssociative = Associativity
    val _ = LeftAssociative
instance Known RightAssociative where
    type Val RightAssociative = Associativity
    val _ = RightAssociative
instance Known NotAssociative where
    type Val NotAssociative = Associativity
    val _ = NotAssociative
instance Known DecidedLazy where
    type Val DecidedLazy = DecidedStrictness
    val _ = DecidedLazy
instance Known DecidedStrict where
    type Val DecidedStrict = DecidedStrictness
    val _ = DecidedStrict
instance Known DecidedUnpack where
    type Val DecidedUnpack = DecidedStrictness
    val _ = DecidedUnpack
instance Known NoSourceStrictness where
    type Val NoSourceStrictness = SourceStrictness
    val _ = NoSourceStrictness
instance Known SourceLazy where
    type Val SourceLazy = SourceStrictness
    val _ = SourceLazy
instance Known SourceStrict where
    type Val SourceStrict = SourceStrictness
    val _ = SourceStrict
instance Known NoSourceUnpackedness where
    type Val NoSourceUnpackedness = SourceUnpackedness
    val _ = NoSourceUnpackedness
instance Known SourceNoUnpack where
    type Val SourceNoUnpack = SourceUnpackedness
    val _ = SourceNoUnpack
instance Known SourceUnpack where
    type Val SourceUnpack = SourceUnpackedness
    val _ = SourceUnpack
#if !MIN_VERSION_base(4, 9, 0)


------------------------------------------------------------------------------
instance
    ( Known n, Val n ~ String, Known m, Val m ~ String, Known nt
    , Val nt ~ Bool
    )
  =>
    Datatype (MetaData n m p nt)
  where
    datatypeName _ = val (Proxy :: Proxy n)
    moduleName _ = val (Proxy :: Proxy m)
#if MIN_VERSION_base(4, 7, 0)
    isNewtype _ = val (Proxy :: Proxy nt)
#endif


------------------------------------------------------------------------------
instance
    (Known n, Val n ~ String, Known f, Val f ~ Fixity, Known r, Val r ~ Bool)
  =>
    Constructor (MetaCons n f r)
  where
    conName _ = val (Proxy :: Proxy n)
    conFixity _ = val (Proxy :: Proxy f)
    conIsRecord _ = val (Proxy :: Proxy r)


------------------------------------------------------------------------------
instance (Known s, Val s ~ String) =>
    Selector (MetaSel (Just s) su ss ds)
  where
    selName _ = val (Proxy :: Proxy s)


------------------------------------------------------------------------------
instance Selector (MetaSel Nothing su ss ds) where
    selName _ = ""
#endif
