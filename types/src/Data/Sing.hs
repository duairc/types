{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#include "kinds.h"

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef KindsAreTypes
{-# LANGUAGE TypeInType #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

#if MIN_VERSION_base(4, 7, 0)
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Data.Sing
    ( Some (Some), fromSome, toSome
    , Sing (Sing), fromSing
    , LoadSing, loadSing, fromJustSing
    , upcast, downcast
    )
where

-- base ----------------------------------------------------------------------
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative, (<*>), pure)
#endif
import           Control.Applicative (Alternative, empty, liftA2)
import           Control.Arrow ((***), first)
import           Data.Bits
                     ( Bits
#if MIN_VERSION_base(4, 7, 0)
                     , FiniteBits
#endif
                     , (.&.)
                     , (.|.)
                     , bit
                     , bitSize
#if MIN_VERSION_base(4, 7, 0)
                     , bitSizeMaybe
#endif
                     , clearBit
                     , complement
                     , complementBit
#if MIN_VERSION_base(4, 8, 0)
                     , countLeadingZeros
                     , countTrailingZeros
#endif
#if MIN_VERSION_base(4, 7, 0)
                     , finiteBitSize
#endif
                     , isSigned
#if MIN_VERSION_base(4, 5, 0)
                     , popCount
#endif
                     , rotate
                     , rotateL
                     , rotateR
                     , setBit
                     , shift
                     , shiftL
                     , shiftR
                     , testBit
#if MIN_VERSION_base(4, 5, 0)
                     , unsafeShiftL
                     , unsafeShiftR
#endif
                     , xor
#if MIN_VERSION_base(4, 7, 0)
                     , zeroBits
#endif
                     )
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Foldable (Foldable, foldr)
#endif
#if MIN_VERSION_base(4, 9, 0)
import           Data.Functor.Classes
                     ( Eq1, liftEq
                     , Ord1, liftCompare
                     , Read1, liftReadList, liftReadsPrec
                     , Show1, liftShowList, liftShowsPrec
                     )
#endif
import           Data.Ix (Ix, range, index, inRange)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid, mappend, mempty)
#endif
#if MIN_VERSION_base(4, 9, 0)
import           Data.Semigroup (Semigroup, (<>))
#endif
import           Data.String (IsString, fromString)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Traversable (Traversable, traverse)
#endif
#if !defined(DataPolyKinds) || defined(PolyTypeable)
import           Data.Typeable (Typeable)
#endif
import           Foreign.Ptr (castPtr)
import           Foreign.Storable (Storable, alignment, peek, poke, sizeOf)
#if !MIN_VERSION_base(4, 8, 0)
import           Prelude hiding (foldr)
#endif
import           Unsafe.Coerce (unsafeCoerce)


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData, rnf)


-- types ---------------------------------------------------------------------
#if defined(GenericDeriving)
import           GHC.Generics.Compat
                     ( Generic, Rep, to, from, Generic1, Rep1, from1, to1
                     , C1, D1, K1 (K1), M1 (M1), S1, Rec0, Par1 (Par1)
                     , DecidedLazy, PrefixI, MetaCons, MetaData, MetaSel
                     , NoSourceStrictness, NoSourceUnpackedness
                     )
import qualified Symbols as S
import           Type.Bool (False)
import           Type.Maybe (Nothing)
#endif
import           Type.Meta
                     ( Known, Val, val
                     , Proxy (Proxy)
#if defined(DataPolyKinds) && !defined(KindsAreTypes)
                     , KProxy
#endif
                     , (:~:) (Refl)
                     , TestEquality, testEquality
                     )


------------------------------------------------------------------------------
data Some TKind (KPoly1) r
    = forall (a :: KPoly1). (Known a, Val a ~ r) => Some !(Proxy a)
#if !defined(DataPolyKinds) || defined(PolyTypeable)
  deriving (Typeable)
#endif


------------------------------------------------------------------------------
instance Functor (Some (TKind (KPoly1))) where
    fmap f (Some a) = toSome (f (val a))


------------------------------------------------------------------------------
instance Applicative (Some (TKind (KPoly1))) where
    pure = toSome
    Some f <*> Some a = toSome (val f (val a))


------------------------------------------------------------------------------
instance Monad (Some (TKind (KPoly1))) where
    return = pure
    Some a >>= f = f (val a)


------------------------------------------------------------------------------
instance Foldable (Some (TKind (KPoly1))) where
    foldr f b (Some a) = f (val a) b


------------------------------------------------------------------------------
instance Traversable (Some (TKind (KPoly1))) where
    traverse f (Some a) = fmap toSome (f (val a))


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance Eq1 (Some (TKind (KPoly1))) where
    liftEq eq (Some a) (Some b) = eq (val a) (val b)


------------------------------------------------------------------------------
instance Ord1 (Some (TKind (KPoly1))) where
    liftCompare cmp (Some a) (Some b) = cmp (val a) (val b)


------------------------------------------------------------------------------
instance Read1 (Some (TKind (KPoly1))) where
    liftReadList _ rl = map (first (map pure)) . rl
    liftReadsPrec rp _ p = map (first pure) . rp p


------------------------------------------------------------------------------
instance Show1 (Some (TKind (KPoly1))) where
    liftShowList _ sl = sl . map (\(Some a) -> val a)
    liftShowsPrec sp _ p = sp p . (\(Some a) -> val a)


#endif
------------------------------------------------------------------------------
instance Eq r => Eq (Some (TKind (KPoly1)) r) where
    Some a == Some b = val a == val b


------------------------------------------------------------------------------
instance Ord r => Ord (Some (TKind (KPoly1)) r) where
    compare (Some a) (Some b) = compare (val a) (val b)


------------------------------------------------------------------------------
instance Read r => Read (Some (TKind (KPoly1)) r) where
    readsPrec p = map (first pure) . readsPrec p


------------------------------------------------------------------------------
instance Show r => Show (Some (TKind (KPoly1)) r) where
    showsPrec p (Some a) = showsPrec p (val a)


------------------------------------------------------------------------------
instance Bounded r => Bounded (Some (TKind (KPoly1)) r) where
    minBound = pure minBound
    maxBound = pure maxBound


------------------------------------------------------------------------------
instance Enum r => Enum (Some (TKind (KPoly1)) r) where
    toEnum = pure . toEnum
    fromEnum (Some r) = fromEnum (val r)


------------------------------------------------------------------------------
instance Ix r => Ix (Some (TKind (KPoly1)) r) where
    range (Some a, Some b) = map pure (range (val a, val b))
    index (Some a, Some b) (Some i) = index (val a, val b) (val i)
    inRange (Some a, Some b) (Some i) = inRange (val a, val b) (val i)


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance Semigroup r => Semigroup (Some (TKind (KPoly1)) r) where
    (<>) = liftA2 (<>)


#endif
------------------------------------------------------------------------------
instance Monoid r => Monoid (Some (TKind (KPoly1)) r) where
    mempty = pure mempty
    mappend = liftA2 mappend


------------------------------------------------------------------------------
instance Storable r => Storable (Some (TKind (KPoly1)) r) where
    sizeOf _ = sizeOf (undefined :: r)
    alignment _ = alignment (undefined :: r)
    peek ptr = fmap pure $ peek (castPtr ptr)
    poke ptr (Some a) = poke (castPtr ptr) (val a)


------------------------------------------------------------------------------
instance Num r => Num (Some (TKind (KPoly1)) r) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger


------------------------------------------------------------------------------
instance Real r => Real (Some (TKind (KPoly1)) r) where
    toRational (Some r) = toRational (val r)


------------------------------------------------------------------------------
instance Integral r => Integral (Some (TKind (KPoly1)) r) where
    quot = liftA2 quot
    rem = liftA2 rem
    div = liftA2 div
    mod = liftA2 mod
    quotRem (Some a) (Some b) =
        pure *** pure $ quotRem (val a) (val b)
    divMod (Some a) (Some b) =
        pure *** pure $ divMod (val a) (val b)
    toInteger (Some a) = toInteger (val a)


------------------------------------------------------------------------------
instance Fractional r => Fractional (Some (TKind (KPoly1)) r) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = pure . fromRational


------------------------------------------------------------------------------
instance RealFrac r => RealFrac (Some (TKind (KPoly1)) r) where
    properFraction (Some x) = (a, pure b)
      where
        (a, b) = properFraction (val x)
    truncate (Some a) = truncate (val a)
    round (Some a) = round (val a)
    ceiling (Some a) = ceiling (val a)
    floor (Some a) = floor (val a)


------------------------------------------------------------------------------
instance Floating r => Floating (Some (TKind (KPoly1)) r) where
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sqrt = fmap sqrt
    (**) = liftA2 (**)
    logBase = liftA2 logBase
    sin = fmap sin
    cos = fmap cos
    tan = fmap tan
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    tanh = fmap tanh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh


------------------------------------------------------------------------------
instance RealFloat r => RealFloat (Some (TKind (KPoly1)) r) where
    floatRadix (Some a) = floatRadix (val a)
    floatDigits (Some a) = floatDigits (val a)
    floatRange (Some a) = floatRange (val a)
    decodeFloat (Some a) = decodeFloat (val a)
    encodeFloat m n = pure (encodeFloat m n)
    exponent (Some a) = exponent (val a)
    significand = fmap significand
    scaleFloat n = fmap (scaleFloat n)
    isNaN (Some a) = isNaN (val a)
    isInfinite (Some a) = isInfinite (val a)
    isDenormalized (Some a) = isDenormalized (val a)
    isNegativeZero (Some a) = isNegativeZero (val a)
    isIEEE (Some a) = isIEEE (val a)
    atan2 = liftA2 atan2


------------------------------------------------------------------------------
instance Bits r => Bits (Some (TKind (KPoly1)) r) where
    (.&.) = liftA2 (.&.)
    (.|.) = liftA2 (.|.)
    xor = liftA2 xor
    complement = fmap complement
    shift a i = fmap (flip shift i) a
    shiftL a i = fmap (flip shiftL i) a
    shiftR a i = fmap (flip shiftR i) a
    rotate a i = fmap (flip shift i) a
    rotateL a i = fmap (flip rotateL i) a
    rotateR a i = fmap (flip rotateR i) a
    bit i = pure (bit i)
    setBit a i = fmap (flip setBit i) a
    clearBit a i = fmap (flip clearBit i) a
    complementBit a i = fmap (flip complementBit i) a
    testBit (Some a) i = testBit (val a) i
    isSigned (Some a) = isSigned (val a)
    bitSize (Some a) = bitSize (val a)
#if MIN_VERSION_base(4, 5, 0)
    unsafeShiftL a i = fmap (flip unsafeShiftL i) a
    unsafeShiftR a i = fmap (flip unsafeShiftR i) a
    popCount (Some a) = popCount (val a)
#endif
#if MIN_VERSION_base(4, 7, 0)
    bitSizeMaybe (Some a) = bitSizeMaybe (val a)
    zeroBits = pure zeroBits


------------------------------------------------------------------------------
instance FiniteBits r => FiniteBits (Some (TKind (KPoly1)) r) where
    finiteBitSize (Some a) = finiteBitSize (val a)
#if MIN_VERSION_base(4, 8, 0)
    countLeadingZeros (Some a) = countLeadingZeros (val a)
    countTrailingZeros (Some a) = countTrailingZeros (val a)
#endif
#endif


------------------------------------------------------------------------------
instance IsString r => IsString (Some (TKind (KPoly1)) r) where
    fromString = toSome . fromString


------------------------------------------------------------------------------
instance NFData r => NFData (Some (TKind (KPoly1)) r) where
    rnf = rnf . fromSome


#ifdef GenericDeriving
------------------------------------------------------------------------------
type SomeD1 = MetaData S.Some S.DataSing S.Types False
type SomeC1 = MetaCons S.Some PrefixI False
type SomeS1 =
    MetaSel Nothing NoSourceUnpackedness NoSourceStrictness DecidedLazy


------------------------------------------------------------------------------
instance Generic (Some (TKind (KPoly1)) r) where
    type Rep (Some (TKind (KPoly1)) r) =
        D1 SomeD1 (C1 SomeC1 (S1 SomeS1 (Rec0 r)))
    from (Some a) = M1 (M1 (M1 (K1 (val a))))
    to (M1 (M1 (M1 (K1 a)))) = pure a


------------------------------------------------------------------------------
instance Generic1 (Some (TKind (KPoly1))) where
    type Rep1 (Some (TKind (KPoly1))) = D1 SomeD1 (C1 SomeC1 (S1 SomeS1 Par1))
    from1 (Some a) = M1 (M1 (M1 (Par1 (val a))))
    to1 (M1 (M1 (M1 (Par1 a)))) = pure a


#endif
------------------------------------------------------------------------------
fromSome :: Some (TKind (KPoly1)) r -> r
fromSome (Some p) = val p


------------------------------------------------------------------------------
toSome :: r -> Some (TKind (KPoly1)) r
toSome = toSomeProxy Proxy
  where
    toSomeProxy :: proxy (TKind (KPoly1)) -> r -> Some (TKind (KPoly1)) r
    toSomeProxy k r = f $ withVal p r Some p
      where
        p = anyProxy k
#if __GLASGOW_HASKELL__ < 700
        f = unsafeCoerce
#else
        f = id
#endif
    anyProxy :: proxy (TKind (KPoly1)) -> Proxy (Any :: KPoly1)
    anyProxy _ = Proxy


------------------------------------------------------------------------------
type family Any :: KPoly1


------------------------------------------------------------------------------
withVal :: forall a b r. Proxy a -> r -> ((Known a, Val a ~ r) => b) -> b
withVal _ r f = unsafeCoerce (withEq (unsafeCoerce Refl) f') (const r)
  where
    f' = EqF (KnownF f) :: EqF r (Val a) (KnownF a b)


------------------------------------------------------------------------------
newtype KnownF a b = KnownF (Known a => b)


------------------------------------------------------------------------------
newtype EqF a b c = EqF ((a ~ b) => c)


------------------------------------------------------------------------------
withEq :: forall a b c. a :~: b -> EqF a b c -> c
withEq Refl (EqF c) = c


------------------------------------------------------------------------------
data Sing r a = (Known a, Val a ~ r) => Sing !(Proxy a)
#if !defined(DataPolyKinds) || defined(PolyTypeable)
  deriving (Typeable)
#endif


------------------------------------------------------------------------------
instance Eq r => TestEquality (Sing r) where
    testEquality (Sing a) (Sing b)
        | val a == val b = Just (unsafeCoerce Refl)
        | otherwise = Nothing


------------------------------------------------------------------------------
instance Eq r => Eq (Sing r a) where
    _ == _ = True


------------------------------------------------------------------------------
instance Ord r => Ord (Sing r a) where
    compare _ _ = EQ


------------------------------------------------------------------------------
instance (Read r, LoadSing r a) => Read (Sing r a) where
    readList s = do
        (rs, s') <- readList s
        pis <- traverse (loadSing (Proxy :: Proxy a)) rs
        return (pis, s')
    readsPrec p s = do
        (r, s') <- readsPrec p s
        pi_ <- loadSing (Proxy :: Proxy a) r
        return (pi_, s')


------------------------------------------------------------------------------
instance Show r => Show (Sing r a) where
    showsPrec p = showsPrec p . fromSing
    showList = showList . map fromSing


------------------------------------------------------------------------------
instance (Bounded r, LoadSing r a) => Bounded (Sing r a) where
    minBound = fromJustSing $ loadSing (Proxy :: Proxy a) minBound
    maxBound = fromJustSing $ loadSing (Proxy :: Proxy a) maxBound


------------------------------------------------------------------------------
instance (Enum r, LoadSing r a) => Enum (Sing r a) where
    toEnum = fromJustSing . loadSing (Proxy :: Proxy a) . toEnum
    fromEnum = fromEnum . fromSing


------------------------------------------------------------------------------
instance (Ix r, LoadSing r a) => Ix (Sing r a) where
    range (a, b) = fromJustSing $ traverse (loadSing (Proxy :: Proxy a)) $
        (range (fromSing a, fromSing b))
    index (a, b) i = index (fromSing a, fromSing b) (fromSing i)
    inRange (a, b) i = inRange (fromSing a, fromSing b) (fromSing i)


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance (Semigroup r, LoadSing r a) => Semigroup (Sing r a) where
    (<>) = lift2 (Proxy :: Proxy a) (<>)


#endif
------------------------------------------------------------------------------
instance (Monoid r, LoadSing r a) => Monoid (Sing r a) where
    mempty = lift0 (Proxy :: Proxy a) mempty
    mappend = lift2 (Proxy :: Proxy a) mappend


------------------------------------------------------------------------------
instance (Storable r, LoadSing r a) => Storable (Sing r a) where
    sizeOf _ = sizeOf (undefined :: r)
    alignment _ = alignment (undefined :: r)
    peek = fmap (fromJustSing . loadSing (Proxy :: Proxy a)) . peek . castPtr
    poke ptr = poke (castPtr ptr) . fromSing


------------------------------------------------------------------------------
instance (Num r, LoadSing r a) => Num (Sing r a) where
    (+) = lift2 (Proxy :: Proxy a) (+)
    (*) = lift2 (Proxy :: Proxy a) (*)
    (-) = lift2 (Proxy :: Proxy a) (-)
    negate = lift1 (Proxy :: Proxy a) negate
    abs = lift1 (Proxy :: Proxy a) abs
    signum = lift1 (Proxy :: Proxy a) signum
    fromInteger = lift0 (Proxy :: Proxy a) . fromInteger


------------------------------------------------------------------------------
instance (Real r, LoadSing r a) => Real (Sing r a) where
    toRational = toRational . fromSing


------------------------------------------------------------------------------
instance (Integral r, LoadSing r a) => Integral (Sing r a) where
    quot = lift2 (Proxy :: Proxy a) quot
    rem = lift2 (Proxy :: Proxy a) rem
    div = lift2 (Proxy :: Proxy a) div
    mod = lift2 (Proxy :: Proxy a) mod
    quotRem a b = lift0 (Proxy :: Proxy a) *** lift0 (Proxy :: Proxy a) $
        quotRem (fromSing a) (fromSing b)
    divMod a b = lift0 (Proxy :: Proxy a) *** lift0 (Proxy :: Proxy a) $
        divMod (fromSing a) (fromSing b)
    toInteger = toInteger . fromSing


------------------------------------------------------------------------------
instance (Fractional r, LoadSing r a) => Fractional (Sing r a) where
    (/) = lift2 (Proxy :: Proxy a) (/)
    recip = lift1 (Proxy :: Proxy a) recip
    fromRational = lift0 (Proxy :: Proxy a) . fromRational


------------------------------------------------------------------------------
instance (RealFrac r, LoadSing r a) => RealFrac (Sing r a) where
    properFraction x = (a, lift0 (Proxy :: Proxy a) b)
      where
        (a, b) = properFraction (fromSing x)
    truncate = truncate . fromSing
    round = round . fromSing
    ceiling = ceiling . fromSing
    floor = floor . fromSing


------------------------------------------------------------------------------
instance (Floating r, LoadSing r a) => Floating (Sing r a) where
    pi = lift0 (Proxy :: Proxy a) pi
    exp = lift1 (Proxy :: Proxy a) exp
    log = lift1 (Proxy :: Proxy a) log
    sqrt = lift1 (Proxy :: Proxy a) sqrt
    (**) = lift2 (Proxy :: Proxy a) (**)
    logBase = lift2 (Proxy :: Proxy a) logBase
    sin = lift1 (Proxy :: Proxy a) sin
    cos = lift1 (Proxy :: Proxy a) cos
    tan = lift1 (Proxy :: Proxy a) tan
    asin = lift1 (Proxy :: Proxy a) asin
    acos = lift1 (Proxy :: Proxy a) acos
    atan = lift1 (Proxy :: Proxy a) atan
    sinh = lift1 (Proxy :: Proxy a) sinh
    cosh = lift1 (Proxy :: Proxy a) cosh
    tanh = lift1 (Proxy :: Proxy a) tanh
    asinh = lift1 (Proxy :: Proxy a) asinh
    acosh = lift1 (Proxy :: Proxy a) acosh
    atanh = lift1 (Proxy :: Proxy a) atanh


------------------------------------------------------------------------------
instance (RealFloat r, LoadSing r a) => RealFloat (Sing r a) where
    floatRadix = floatRadix . fromSing
    floatDigits = floatDigits . fromSing
    floatRange = floatRange . fromSing
    decodeFloat = decodeFloat . fromSing
    encodeFloat m n = lift0 (Proxy :: Proxy a) (encodeFloat m n)
    exponent = exponent . fromSing
    significand = lift1 (Proxy :: Proxy a) significand
    scaleFloat n = lift1 (Proxy :: Proxy a) (scaleFloat n)
    isNaN = isNaN . fromSing
    isInfinite = isInfinite . fromSing
    isDenormalized = isDenormalized . fromSing
    isNegativeZero = isNegativeZero . fromSing
    isIEEE = isIEEE . fromSing
    atan2 = lift2 (Proxy :: Proxy a) atan2


------------------------------------------------------------------------------
instance (Bits r, LoadSing r a) => Bits (Sing r a) where
    (.&.) = lift2 (Proxy :: Proxy a) (.&.)
    (.|.) = lift2 (Proxy :: Proxy a) (.|.)
    xor = lift2 (Proxy :: Proxy a) xor
    complement = lift1 (Proxy :: Proxy a) complement
    shift a i = lift1 (Proxy :: Proxy a) (flip shift i) a
    shiftL a i = lift1 (Proxy :: Proxy a) (flip shiftL i) a
    shiftR a i = lift1 (Proxy :: Proxy a) (flip shiftR i) a
    rotate a i = lift1 (Proxy :: Proxy a) (flip shift i) a
    rotateL a i = lift1 (Proxy :: Proxy a) (flip rotateL i) a
    rotateR a i = lift1 (Proxy :: Proxy a) (flip rotateR i) a
    bit i = lift0 (Proxy :: Proxy a) (bit i)
    setBit a i = lift1 (Proxy :: Proxy a) (flip setBit i) a
    clearBit a i = lift1 (Proxy :: Proxy a) (flip clearBit i) a
    complementBit a i = lift1 (Proxy :: Proxy a) (flip complementBit i) a
    testBit a i = testBit (fromSing a) i
    isSigned = isSigned . fromSing
    bitSize = bitSize . fromSing
#if MIN_VERSION_base(4, 5, 0)
    unsafeShiftL a i = lift1 (Proxy :: Proxy a) (flip unsafeShiftL i) a
    unsafeShiftR a i = lift1 (Proxy :: Proxy a) (flip unsafeShiftR i) a
    popCount = popCount . fromSing
#endif
#if MIN_VERSION_base(4, 7, 0)
    bitSizeMaybe = bitSizeMaybe . fromSing
    zeroBits = lift0 (Proxy :: Proxy a) zeroBits


------------------------------------------------------------------------------
instance (FiniteBits r, LoadSing r a) => FiniteBits (Sing r a) where
    finiteBitSize = finiteBitSize . fromSing
#if MIN_VERSION_base(4, 8, 0)
    countLeadingZeros = countLeadingZeros . fromSing
    countTrailingZeros = countTrailingZeros . fromSing
#endif
#endif


------------------------------------------------------------------------------
instance (IsString r, LoadSing r a) => IsString (Sing r a) where
    fromString = fromJustSing . loadSing (Proxy :: Proxy a) . fromString


------------------------------------------------------------------------------
instance NFData r => NFData (Sing r a) where
    rnf = rnf . fromSing
#ifdef GenericDeriving


------------------------------------------------------------------------------
type SingD1 = MetaData S.Sing S.DataSing S.Types False
type SingC1 = MetaCons S.Sing PrefixI False
type SingS1 =
    MetaSel Nothing NoSourceUnpackedness NoSourceStrictness DecidedLazy


------------------------------------------------------------------------------
instance LoadSing r a => Generic (Sing r a) where
    type Rep (Sing r a) = D1 SingD1 (C1 SingC1 (S1 SingS1 (Rec0 r)))
    from = M1 . M1 . M1 . K1 . fromSing
    to (M1 (M1 (M1 (K1 a)))) = lift0 (Proxy :: Proxy a) a
#endif


------------------------------------------------------------------------------
fromSing :: Sing r a -> r
fromSing (Sing p) = val p


------------------------------------------------------------------------------
class
    ( Known a
#ifdef EqualitySuperclassConstraints
    , Val a ~ r
#endif
    , Eq r
    )
  =>
    LoadSing r a
  where
    loadSing :: Alternative f => proxy a -> r -> f (Sing r a)


------------------------------------------------------------------------------
instance (Known a, Val a ~ r, Eq r) => LoadSing r a where
    loadSing p a
        | val p == a = pure (Sing Proxy)
        | otherwise = empty
    {-# INLINE loadSing #-}


------------------------------------------------------------------------------
fromJustSing :: Maybe a -> a
fromJustSing (Just a) = a
fromJustSing Nothing = error "Data.Sing.loadSing: value did not match type"
{-# INLINE fromJustSing #-}


------------------------------------------------------------------------------
lift0 :: LoadSing r a => proxy a -> r -> Sing r a
lift0 p = fromJustSing . loadSing p


------------------------------------------------------------------------------
lift1 :: LoadSing r a => proxy a -> (r -> r) -> Sing r a -> Sing r a
lift1 p f = fromJustSing . loadSing p . f . fromSing


------------------------------------------------------------------------------
lift2 :: LoadSing r a
    => proxy a -> (r -> r -> r) -> Sing r a -> Sing r a -> Sing r a
lift2 p f a b = fromJustSing $ loadSing p $ f (fromSing a) (fromSing b)


------------------------------------------------------------------------------
upcast :: Sing r (a :: KPoly1) -> Some (TKind (KPoly1)) r
upcast (Sing p) = Some p


------------------------------------------------------------------------------
downcast
    :: Some (TKind (KPoly1)) r
    -> (forall a. (Known a, Val a ~ r) => Sing r (a :: KPoly1) -> b)
    -> b
downcast (Some p) f = f (Sing p)
