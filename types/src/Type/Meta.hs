{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#include "kinds.h"

#ifdef GenericDeriving
{-# LANGUAGE DeriveGeneric #-}
#endif

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

#ifdef KindsAreTypes
{-# LANGUAGE TypeInType #-}
#endif

#if MIN_VERSION_base(4, 7, 0)
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Type.Meta
    ( Known, Val, val
    , same
    , Proxy (Proxy), KProxy (KProxy)
    , Void, absurd
    , (:~:) (Refl), TestEquality, testEquality
    )
where

-- base ----------------------------------------------------------------------
#if !MIN_VERSION_base(4, 7, 0)
import           Control.Applicative (Applicative, pure, (<*>))
#endif
#if !MIN_VERSION_base(4, 7, 0)
import           Control.Category (Category, id, (.))
#endif
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Exception (Exception)
#endif
#if !MIN_VERSION_base(4, 7, 0)
import           Data.Foldable
                     ( Foldable, foldMap, fold, foldl, foldr, foldl1, foldr1
                     )
#endif
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Ix (Ix, range, index, inRange, rangeSize)
#endif
#if !MIN_VERSION_base(4, 7, 0)
import           Data.Monoid (Monoid, mappend, mempty, mconcat)
#endif
#if MIN_VERSION_base(4, 7, 0)
import           Data.Proxy (Proxy (Proxy), KProxy (KProxy))
#endif
#if !MIN_VERSION_base(4, 7, 0)
import           Data.Traversable
                     ( Traversable, traverse, sequenceA, mapM, sequence
                     )
#endif
#if MIN_VERSION_base(4, 7, 0)
import           Data.Type.Equality ((:~:) (Refl), TestEquality, testEquality)
#endif
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Typeable (Typeable)
#endif
#if MIN_VERSION_base(4, 8, 0)
import           Data.Void (Void, absurd)
#endif
#if defined(GenericDeriving) && !MIN_VERSION_base(4, 8, 0)
import           GHC.Generics
                     ( Generic
#if !MIN_VERSION_base(4, 7, 0)
                     , Rep, to, from, Generic1, Rep1, from1, to1
                     , C1, D1, M1 (M1), U1 (U1)
                     , Datatype, Constructor
                     , datatypeName, moduleName, conName
#endif
                     )
#endif
#if MIN_VERSION_base(4, 6, 0) && defined(DataPolyKinds)
import           GHC.TypeLits
                     ( Nat
                     , Symbol
#if MIN_VERSION_base(4, 7, 0)
                     , KnownNat
                     , natVal
                     , KnownSymbol
                     , symbolVal
#else
                     , SingRep
                     , fromSing
                     , sing
                     )
import qualified GHC.TypeLits as G
                     ( Sing
#endif
                     )
#endif
#if MIN_VERSION_base(4, 8, 0) && defined (DataPolyKinds)
import           Numeric.Natural (Natural)
#endif
#if !MIN_VERSION_base(4, 7, 0)
import           Prelude hiding
                     ( (.)
                     , foldl
                     , foldr
                     , foldl1
                     , foldr1
                     , id
                     , mapM
                     , sequence
                     )
#endif
import           Unsafe.Coerce (unsafeCoerce)
#if !MIN_VERSION_base(4, 8, 0)


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData, rnf)
#endif


------------------------------------------------------------------------------
class Known t where
    type Val t
    val :: proxy t -> Val t


#ifdef DataPolyKinds
------------------------------------------------------------------------------
instance Known 'True where
    type Val 'True = Bool
    val _ = True
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance Known 'False where
    type Val 'False = Bool
    val _ = False
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance Known 'LT where
    type Val 'LT = Ordering
    val _ = LT
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance Known 'EQ where
    type Val 'EQ = Ordering
    val _ = EQ
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance Known 'GT where
    type Val 'GT = Ordering
    val _ = GT
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance Known '() where
    type Val '() = ()
    val _ = ()
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance (Known a, Known b) => Known '(a, b) where
    type Val '(a, b) = (Val a, Val b)
    val _ = (val (Proxy :: Proxy a), val (Proxy :: Proxy b))
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance (Known a, Known b, Known c) => Known '(a, b, c) where
    type Val '(a, b, c) = (Val a, Val b, Val c)
    val _ = (val (Proxy :: Proxy a), val (Proxy :: Proxy b),
        val (Proxy :: Proxy c))
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance (Known a, Known b, Known c, Known d) => Known '(a, b, c, d) where
    type Val '(a, b, c, d) = (Val a, Val b, Val c, Val d)
    val _ = (val (Proxy :: Proxy a), val (Proxy :: Proxy b),
        val (Proxy :: Proxy c), val (Proxy :: Proxy d))
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance (Known a, Known b, Known c, Known d, Known e) =>
    Known '(a, b, c, d, e)
  where
    type Val '(a, b, c, d, e) = (Val a, Val b, Val c, Val d, Val e)
    val _ = (val (Proxy :: Proxy a), val (Proxy :: Proxy b),
        val (Proxy :: Proxy c), val (Proxy :: Proxy d),
        val (Proxy :: Proxy e))
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance (Known a, Known b, Known c, Known d, Known e, Known f) =>
    Known '(a, b, c, d, e, f)
  where
    type Val '(a, b, c, d, e, f) = (Val a, Val b, Val c, Val d, Val e, Val f)
    val _ = (val (Proxy :: Proxy a), val (Proxy :: Proxy b),
        val (Proxy :: Proxy c), val (Proxy :: Proxy d),
        val (Proxy :: Proxy e), val (Proxy :: Proxy f))
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance (Known a, Known b, Known c, Known d, Known e, Known f, Known g) =>
    Known '(a, b, c, d, e, f, g)
  where
    type Val '(a, b, c, d, e, f, g) =
        (Val a, Val b, Val c, Val d, Val e, Val f, Val g)
    val _ = (val (Proxy :: Proxy a), val (Proxy :: Proxy b),
        val (Proxy :: Proxy c), val (Proxy :: Proxy d),
        val (Proxy :: Proxy e), val (Proxy :: Proxy f),
        val (Proxy :: Proxy g))
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance Known ('[] :: [k]) where
    type Val ('[] :: [k]) = [Void]
    val _ = []
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance Known a => Known '[a] where
    type Val '[a] = [Val a]
    val _ = [val (Proxy :: Proxy a)]
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance (Known a, Known (a' ': as), Val (a' ': as) ~ [Val a]) =>
    Known (a ': (a' ': as))
  where
    type Val (a ': (a' ': as)) = [Val a]
    val _ = val (Proxy :: Proxy a) : val (Proxy :: Proxy (a' ': as))
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance Known ('Nothing :: Maybe k) where
    type Val ('Nothing :: Maybe k) = Maybe Void
    val _ = Nothing
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance Known a => Known ('Just a) where
    type Val ('Just a) = Maybe (Val a)
    val _ = Just (val (Proxy :: Proxy a))
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance Known a => Known ('Left a :: Either k k) where
    type Val ('Left a :: Either k k) = Either (Val a) Void
    val _ = Left (val (Proxy :: Proxy a))
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance Known a => Known ('Right a :: Either k k) where
    type Val ('Right a :: Either k k) = Either Void (Val a)
    val _ = Right (val (Proxy :: Proxy a))
    {-# INLINE val #-}
#if MIN_VERSION_base(4, 6, 0)


------------------------------------------------------------------------------
instance
#if MIN_VERSION_base(4, 7, 0)
    KnownSymbol a
#else
    SingRep a String
#endif
  =>
    Known (a :: Symbol)
  where
    type Val (a :: Symbol) = String
#if MIN_VERSION_base(4, 7, 0)
    val = symbolVal
#else
    val _ = fromSing (sing :: G.Sing a)
#endif
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance
#if MIN_VERSION_base(4, 7, 0)
    KnownNat a
#else
    SingRep a Integer
#endif
  =>
    Known (a :: Nat)
  where
#if MIN_VERSION_base(4, 8, 0)
    type Val (a :: Nat) = Natural
#else
    type Val (a :: Nat) = Integer
#endif
#if MIN_VERSION_base(4, 7, 0)
    val = fromInteger . natVal
#else
    val _ = fromSing (sing :: G.Sing a)
#endif
    {-# INLINE val #-}
#endif
#endif
#if !MIN_VERSION_base(4, 7, 0)


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
#endif


------------------------------------------------------------------------------
same :: (Eq (Val a), Val a ~ Val b, Known a, Known b)
    => proxy a
    -> proxy' b
    -> Maybe (a :~: b)
same a b = if val a == val b then Just (unsafeCoerce Refl) else Nothing
#if !MIN_VERSION_base(4, 7, 0)


------------------------------------------------------------------------------
data Proxy a = Proxy
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Bounded
#if !defined(DataPolyKinds) || defined(PolyTypeable)
    , Typeable
#endif
    )


------------------------------------------------------------------------------
instance Functor Proxy where
    fmap _ _ = Proxy
    {-# INLINE fmap #-}


------------------------------------------------------------------------------
instance Applicative Proxy where
    pure _ = Proxy
    {-# INLINE pure #-}
    _ <*> _ = Proxy
    {-# INLINE (<*>) #-}


------------------------------------------------------------------------------
instance Monad Proxy where
    return _ = Proxy
    {-# INLINE return #-}
    _ >>= _ = Proxy
    {-# INLINE (>>=) #-}


------------------------------------------------------------------------------
instance Foldable Proxy where
    foldMap _ _ = mempty
    {-# INLINE foldMap #-}
    fold _ = mempty
    {-# INLINE fold #-}
    foldl _ z _ = z
    {-# INLINE foldl #-}
    foldr _ z _ = z
    {-# INLINE foldr #-}
    foldl1 _ _ = error "foldl1: Proxy"
    {-# INLINE foldl1 #-}
    foldr1 _ _ = error "foldr1: Proxy"
    {-# INLINE foldr1 #-}


------------------------------------------------------------------------------
instance Traversable Proxy where
    traverse _ _ = pure Proxy
    {-# INLINE traverse #-}
    sequenceA _ = pure Proxy
    {-# INLINE sequenceA #-}
    mapM _ _ = return Proxy
    {-# INLINE mapM #-}
    sequence _ = return Proxy
    {-# INLINE sequence #-}


------------------------------------------------------------------------------
instance Enum (Proxy a) where
    toEnum 0 = Proxy
    toEnum _ = error "Type.Meta.Proxy.toEnum: bad argument"
    fromEnum Proxy = 0


------------------------------------------------------------------------------
instance Ix (Proxy a) where
    range _ = [Proxy]
    index _ _ = 0
    inRange _ _ = True
    rangeSize _ = 1


------------------------------------------------------------------------------
instance Monoid (Proxy a) where
    mempty = Proxy
    mappend _ _ = Proxy
    mconcat _ = Proxy


------------------------------------------------------------------------------
instance NFData (Proxy a) where
    rnf Proxy = ()
#ifdef GenericDeriving


------------------------------------------------------------------------------
data ProxyD1
data ProxyC1


------------------------------------------------------------------------------
instance Datatype ProxyD1 where
    datatypeName _ = "Proxy"
    moduleName _ = "Type.Meta"


------------------------------------------------------------------------------
instance Constructor ProxyC1 where
    conName _ = "Proxy"


------------------------------------------------------------------------------
instance Generic (Proxy a) where
    type Rep (Proxy a) = D1 ProxyD1 (C1 ProxyC1 U1)
    from _ = M1 (M1 U1)
    to _ = Proxy


------------------------------------------------------------------------------
instance Generic1 Proxy where
    type Rep1 Proxy = D1 ProxyD1 (C1 ProxyC1 U1)
    from1 _ = M1 (M1 U1)
    to1 _ = Proxy
#endif


------------------------------------------------------------------------------
data KProxy (t :: *) = KProxy
#endif
#if !MIN_VERSION_base(4, 8, 0)


------------------------------------------------------------------------------
data Void
  deriving
    ( Typeable
#ifdef GenericDeriving
    , Generic
#endif
    )


------------------------------------------------------------------------------
instance Eq Void where
    _ == _ = True


------------------------------------------------------------------------------
instance Ord Void where
    compare _ _ = EQ


------------------------------------------------------------------------------
instance Ix Void where
    range _     = []
    index _     = absurd
    inRange _   = absurd
    rangeSize _ = 0


------------------------------------------------------------------------------
instance Read Void where
    readsPrec _ _ = []


------------------------------------------------------------------------------
instance Show Void where
    showsPrec _ = absurd


------------------------------------------------------------------------------
instance Exception Void


------------------------------------------------------------------------------
instance NFData Void where
    rnf = absurd


------------------------------------------------------------------------------
absurd :: Void -> a
absurd _ = undefined
#endif
