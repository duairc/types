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

#if __GLASGOW_HASKELL__ >= 711
{-# LANGUAGE TypeInType #-}
#endif

#include "kinds.h"

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 704
-- disable optimisation on GHC 7.2 to prevent panic (possibly #5315 ?)
{-# OPTIONS_GHC -O0 #-}
#endif

module Type.Meta
    ( Known
    , Val
    , val
    , Some (Some)
    , someVal
    , same
    , Proxy (Proxy)
    , Void
    , absurd
    , (:~:) (Refl)
    , TestEquality
    , testEquality
    )
where

-- base ----------------------------------------------------------------------
#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative (Applicative, pure, (<*>))
#endif
import           Control.Arrow ((***))
#if __GLASGOW_HASKELL__ < 708
import           Control.Category (Category, id, (.))
#endif
#if __GLASGOW_HASKELL__ < 710
import           Control.Exception (Exception)
#endif
#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable
                     ( Foldable
                     , foldMap
#if __GLASGOW_HASKELL__ < 708
                     , fold
                     , foldl
                     , foldr
                     , foldl1
                     , foldr1
#endif
                     )
#endif
import           Data.Ix
                     ( Ix
                     , range
                     , index
                     , inRange
#if __GLASGOW_HASKELL__ < 710
                     , rangeSize
#endif
                     )
#if __GLASGOW_HASKELL__ >= 711
import           Data.Kind (Type)
#endif
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid
                     ( Monoid
                     , mappend
                     , mempty
#if __GLASGOW_HASKELL__ < 708
                     , mconcat
#endif
                     )
#endif
#if __GLASGOW_HASKELL__ >= 708
import           Data.Proxy (Proxy (Proxy))
#endif
#if __GLASGOW_HASKELL__ >= 711
import           Data.Semigroup (Semigroup, (<>))
#endif
#if __GLASGOW_HASKELL__ < 710
import           Data.Traversable
                     ( Traversable
                     , traverse
#if __GLASGOW_HASKELL__ < 708
                     , sequenceA
                     , mapM
                     , sequence
#endif
                     )
#endif
#if __GLASGOW_HASKELL__ >= 708
import           Data.Type.Equality ((:~:) (Refl), TestEquality, testEquality)
#endif
#if __GLASGOW_HASKELL__ < 710
import           Data.Typeable (Typeable)
#endif
#if __GLASGOW_HASKELL__ >= 710
import           Data.Void (Void, absurd)
#endif
#if defined(GenericDeriving) && __GLASGOW_HASKELL__ < 710
import           GHC.Generics (Generic)
#endif
#if __GLASGOW_HASKELL__ >= 706 && defined(DataPolyKinds)
import           GHC.TypeLits
                     ( Nat
                     , Symbol
#if __GLASGOW_HASKELL__ >= 708
                     , KnownNat
                     , natVal
                     , KnownSymbol
                     , symbolVal
#else
                     , Sing
                     , SingRep
                     , fromSing
                     , sing
#endif
                     )
#endif
#if __GLASGOW_HASKELL__ >= 710 && defined (DataPolyKinds)
import           Numeric.Natural (Natural)
#endif
#if __GLASGOW_HASKELL__ < 708
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
#if __GLASGOW_HASKELL__ >= 706


------------------------------------------------------------------------------
instance
#if __GLASGOW_HASKELL__ >= 708
    KnownSymbol a
#else
    SingRep a String
#endif
  =>
    Known (a :: Symbol)
  where
    type Val (a :: Symbol) = String
#if __GLASGOW_HASKELL__ >= 708
    val = symbolVal
#else
    val _ = fromSing (sing :: Sing a)
#endif
    {-# INLINE val #-}


------------------------------------------------------------------------------
instance
#if __GLASGOW_HASKELL__ >= 708
    KnownNat a
#else
    SingRep a Integer
#endif
  =>
    Known (a :: Nat)
  where
#if __GLASGOW_HASKELL__ >= 710
    type Val (a :: Nat) = Natural
#else
    type Val (a :: Nat) = Integer
#endif
#if __GLASGOW_HASKELL__ >= 708
    val = fromInteger . natVal
#else
    val _ = fromSing (sing :: Sing a)
#endif
    {-# INLINE val #-}
#endif
#endif


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 711
data Some r = forall (k :: Type) (a :: k). (Known a, Val a ~ r) =>
    Some (Proxy (a :: k))
#else
data Some r = forall a. (Known a, Val a ~ r) => Some (Proxy a)
#endif


------------------------------------------------------------------------------
instance Functor Some where
    fmap f (Some a) = someVal (f (val a))


------------------------------------------------------------------------------
instance Applicative Some where
    pure = someVal
    Some f <*> Some a = someVal (val f (val a))


------------------------------------------------------------------------------
instance Monad Some where
    return = pure
    Some a >>= f = f (val a)


------------------------------------------------------------------------------
instance Foldable Some where
    foldMap f (Some a) = f (val a)


------------------------------------------------------------------------------
instance Traversable Some where
    traverse f (Some a) = fmap someVal (f (val a))


------------------------------------------------------------------------------
instance Eq r => Eq (Some r) where
    Some a == Some b = val a == val b


------------------------------------------------------------------------------
instance Ord r => Ord (Some r) where
    compare (Some a) (Some b) = compare (val a) (val b)


------------------------------------------------------------------------------
instance Read r => Read (Some r) where
    readsPrec p xs = do
        (a, ys) <- readsPrec p xs
        return (someVal a, ys)


------------------------------------------------------------------------------
instance Show r => Show (Some r) where
    showsPrec p (Some a) = showsPrec p (val a)


------------------------------------------------------------------------------
instance Bounded r => Bounded (Some r) where
    minBound = someVal minBound
    maxBound = someVal maxBound


------------------------------------------------------------------------------
instance Enum r => Enum (Some r) where
    toEnum = someVal . toEnum
    fromEnum (Some r) = fromEnum (val r)


------------------------------------------------------------------------------
instance Ix r => Ix (Some r) where
    range (Some a, Some b) = map someVal (range (val a, val b))
    index (Some a, Some b) (Some i) = index (val a, val b) (val i)
    inRange (Some a, Some b) (Some i) = inRange (val a, val b) (val i)


#if __GLASGOW_HASKELL__ >= 711
------------------------------------------------------------------------------
instance Semigroup r => Semigroup (Some r) where
    Some a <> Some b = someVal (val a <> val b)


#endif
------------------------------------------------------------------------------
instance Monoid r => Monoid (Some r) where
    mempty = someVal mempty
    mappend (Some a) (Some b) = someVal (mappend (val a) (val b))


------------------------------------------------------------------------------
instance Num r => Num (Some r) where
    Some a + Some b = someVal (val a + val b)
    Some a * Some b = someVal (val a * val b)
    Some a - Some b = someVal (val a - val b)
    negate (Some a) = someVal (negate (val a))
    abs (Some a) = someVal (abs (val a))
    signum (Some a) = someVal (signum (val a))
    fromInteger = someVal . fromInteger


------------------------------------------------------------------------------
instance Real r => Real (Some r) where
    toRational (Some r) = toRational (val r)


------------------------------------------------------------------------------
instance Integral r => Integral (Some r) where
    quot (Some a) (Some b) = someVal (quot (val a) (val b))
    rem (Some a) (Some b) = someVal (rem (val a) (val b))
    div (Some a) (Some b) = someVal (div (val a) (val b))
    mod (Some a) (Some b) = someVal (mod (val a) (val b))
    quotRem (Some a) (Some b) = someVal *** someVal $ quotRem (val a) (val b)
    divMod (Some a) (Some b) = someVal *** someVal $ divMod (val a) (val b)
    toInteger (Some a) = toInteger (val a)


------------------------------------------------------------------------------
instance Fractional r => Fractional (Some r) where
    Some a / Some b = someVal (val a / val b)
    recip (Some a) = someVal (recip (val a))
    fromRational = someVal . fromRational


------------------------------------------------------------------------------
instance Floating r => Floating (Some r) where
    pi = someVal pi
    exp (Some a) = someVal (exp (val a))
    log (Some a) = someVal (log (val a))
    sqrt (Some a) = someVal (sqrt (val a))
    Some a ** Some b = someVal (val a ** val b)
    logBase (Some a) (Some b) = someVal (logBase (val a) (val b))
    sin (Some a) = someVal (sin (val a))
    cos (Some a) = someVal (cos (val a))
    tan (Some a) = someVal (tan (val a))
    asin (Some a) = someVal (asin (val a))
    acos (Some a) = someVal (acos (val a))
    atan (Some a) = someVal (atan (val a))
    sinh (Some a) = someVal (sinh (val a))
    cosh (Some a) = someVal (cosh (val a))
    tanh (Some a) = someVal (tanh (val a))
    asinh (Some a) = someVal (asinh (val a))
    acosh (Some a) = someVal (acosh (val a))
    atanh (Some a) = someVal (atanh (val a))


------------------------------------------------------------------------------
someVal :: r -> Some r
someVal r =
#if __GLASGOW_HASKELL__ < 700
    unsafeCoerce $
#endif
    withVal p r Some p
  where
    p = Proxy :: Proxy Any


------------------------------------------------------------------------------
type family Any


------------------------------------------------------------------------------
same :: (Eq (Val a), Val a ~ Val b, Known a, Known b)
    => proxy a
    -> proxy' b
    -> Maybe (a :~: b)
same a b = if val a == val b then Just (unsafeCoerce Refl) else Nothing


------------------------------------------------------------------------------
newtype KnownF r a b = KnownF ((Known a, Val a ~ r) => b)


------------------------------------------------------------------------------
withVal :: forall a b r. Proxy a -> r -> ((Known a, Val a ~ r) => b) -> b
withVal _ r f = unsafeCoerce (KnownF f :: KnownF r a b) (const r)
{-# INLINE withVal #-}
#if __GLASGOW_HASKELL__ < 708


------------------------------------------------------------------------------
data a :~: b where
    Refl :: a :~: a
#ifndef DataPolyKinds
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
data Proxy a = Proxy
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Bounded
#ifdef GenericDeriving
    , Generic
#endif
#if !defined(DataPolyKinds) || __GLASGOW_HASKELL__ >= 708
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


#endif
#if __GLASGOW_HASKELL__ < 710
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
absurd :: Void -> a
absurd _ = undefined
#endif
