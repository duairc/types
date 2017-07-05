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

module Data.Pi
    ( Pi (Pi, Some), fromPi, toPi
    , LoadPi (loadPi), fromJustPi
    , upcast, topcast, downcast, bottomcast
    )
where

-- base ----------------------------------------------------------------------
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (pure)
#endif
import           Control.Applicative (Alternative, empty)
import           Control.Arrow ((***))
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
import           Data.String (IsString, fromString)
import           Data.Ix (Ix, range, index, inRange)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid, mappend, mempty)
#endif
#if MIN_VERSION_base(4, 9, 0)
import           Data.Semigroup (Semigroup, (<>))
#endif
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Traversable (traverse)
#endif
#if !defined(DataPolyKinds) || defined(PolyTypeable)
import           Data.Typeable (Typeable)
#endif
import           Foreign.Ptr (castPtr)
import           Foreign.Storable (Storable, alignment, peek, poke, sizeOf)
#if defined(GenericDeriving)
import           GHC.Generics
                     ( Generic, Rep, to, from
                     , C1, D1, K1 (K1), M1 (M1), S1, Rec0
#if MIN_VERSION_base(4, 9, 0)
                     , DecidedStrictness (DecidedLazy)
                     , FixityI (PrefixI)
                     , Meta (MetaCons, MetaData, MetaSel)
                     , SourceStrictness (NoSourceStrictness)
                     , SourceUnpackedness (NoSourceUnpackedness)
#else
                     , Datatype, Constructor, NoSelector
                     , datatypeName, moduleName, conName
#endif
                     )
#endif
#if !MIN_VERSION_base(4, 8, 0)
import           Prelude hiding (foldr)
#endif
import           Unsafe.Coerce (unsafeCoerce)


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData, rnf)


-- types ---------------------------------------------------------------------
import           Type.Maybe (Just, Nothing)
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
data Pi TKind (KPoly1) r (a :: KMaybe (KPoly1)) where
    Pi
        :: (Known (a :: KPoly1), Val a ~ r)
        => !(Proxy a)
        -> Pi (TKind (KPoly1)) r (Just a)
    Some
        :: (Known (b :: KPoly1), Val b ~ r)
        => !(Proxy b)
        -> Pi (TKind (KPoly1)) r Nothing
#if !defined(DataPolyKinds) || defined(PolyTypeable)
  deriving (Typeable)
#endif


------------------------------------------------------------------------------
fromPi :: Pi (TKind (KPoly1)) r a -> r
fromPi (Pi p) = val p
fromPi (Some p) = val p


------------------------------------------------------------------------------
toPi :: r -> Pi (TKind (KPoly1)) r Nothing
toPi = toPiProxy Proxy
  where
    toPiProxy :: proxy (TKind (KPoly1)) -> r -> Pi (TKind (KPoly1)) r Nothing
    toPiProxy k r = f $ withVal p r Some p
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
lift0
    :: LoadPi (TKind (KPoly1)) r a
    => proxy a
    -> r
    -> Pi (TKind (KPoly1)) r a
lift0 p = fromJustPi . loadPi p


------------------------------------------------------------------------------
lift1
    :: LoadPi (TKind (KPoly1)) r a
    => proxy a
    -> (r -> r)
    -> Pi (TKind (KPoly1)) r a
    -> Pi (TKind (KPoly1)) r a
lift1 p f = fromJustPi . loadPi p . f . fromPi


------------------------------------------------------------------------------
lift2
    :: LoadPi (TKind (KPoly1)) r a
    => proxy a
    -> (r -> r -> r)
    -> Pi (TKind (KPoly1)) r a
    -> Pi (TKind (KPoly1)) r a
    -> Pi (TKind (KPoly1)) r a
lift2 p f a b = fromJustPi $ loadPi p $ f (fromPi a) (fromPi b)


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
class LoadPi TKind (KPoly1) r (a :: KMaybe (KPoly1)) where
    loadPi :: Alternative f => proxy a -> r -> f (Pi (TKind (KPoly1)) r a)


------------------------------------------------------------------------------
instance LoadPi (TKind (KPoly1)) r Nothing where
    loadPi _ = pure . toPi
    {-# INLINE loadPi #-}


------------------------------------------------------------------------------
instance (Known a, Val a ~ r, Eq r) => LoadPi (TKind (KPoly1)) r (Just a)
  where
    loadPi _ a
        | val (Proxy :: Proxy a) == a = pure (Pi Proxy)
        | otherwise = empty
    {-# INLINE loadPi #-}


------------------------------------------------------------------------------
fromJustPi :: Maybe a -> a
fromJustPi (Just a) = a
fromJustPi Nothing = error "Data.Pi.loadPi: value did not match type"
{-# INLINE fromJustPi #-}


------------------------------------------------------------------------------
instance Eq r => Eq (Pi (TKind (KPoly1)) r a) where
    Pi _ == Pi _ = True
    Some a == Some b = val a == val b
#if __GLASGOW_HASKELL__ < 800
    _ == _ = undefined
#endif


------------------------------------------------------------------------------
instance Ord r => Ord (Pi (TKind (KPoly1)) r a) where
    compare (Pi _) (Pi _) = EQ
    compare (Some a) (Some b) = compare (val a) (val b)
#if __GLASGOW_HASKELL__ < 800
    compare _ _ = undefined
#endif


------------------------------------------------------------------------------
instance (Read r, LoadPi (TKind (KPoly1)) r a) =>
    Read (Pi (TKind (KPoly1)) r a)
  where
    readList s = do
        (rs, s') <- readList s
        pis <- traverse (loadPi (Proxy :: Proxy a)) rs
        return (pis, s')
    readsPrec p s = do
        (r, s') <- readsPrec p s
        pi_ <- loadPi (Proxy :: Proxy a) r
        return (pi_, s')


------------------------------------------------------------------------------
instance Show r => Show (Pi (TKind (KPoly1)) r a) where
    showsPrec p = showsPrec p . fromPi
    showList = showList . map fromPi


------------------------------------------------------------------------------
instance (Bounded r, LoadPi (TKind (KPoly1)) r a) =>
    Bounded (Pi (TKind (KPoly1)) r a)
  where
    minBound = fromJustPi $ loadPi (Proxy :: Proxy a) minBound
    maxBound = fromJustPi $ loadPi (Proxy :: Proxy a) maxBound


------------------------------------------------------------------------------
instance (Enum r, LoadPi (TKind (KPoly1)) r a) =>
    Enum (Pi (TKind (KPoly1)) r a)
  where
    toEnum = fromJustPi . loadPi (Proxy :: Proxy a) . toEnum
    fromEnum = fromEnum . fromPi


------------------------------------------------------------------------------
instance (Ix r, LoadPi (TKind (KPoly1)) r a) => Ix (Pi (TKind (KPoly1)) r a)
   where
    range (a, b) = fromJustPi $ traverse (loadPi (Proxy :: Proxy a)) $
        (range (fromPi a, fromPi b))
    index (a, b) i = index (fromPi a, fromPi b) (fromPi i)
    inRange (a, b) i = inRange (fromPi a, fromPi b) (fromPi i)


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance (Semigroup r, LoadPi (TKind (KPoly1)) r a) =>
    Semigroup (Pi (TKind (KPoly1)) r a)
  where
    (<>) = lift2 (Proxy :: Proxy a) (<>)


#endif
------------------------------------------------------------------------------
instance (Monoid r, LoadPi (TKind (KPoly1)) r a) =>
    Monoid (Pi (TKind (KPoly1)) r a)
  where
    mempty = lift0 (Proxy :: Proxy a) mempty
    mappend = lift2 (Proxy :: Proxy a) mappend


------------------------------------------------------------------------------
instance (Storable r, LoadPi (TKind (KPoly1)) r a) =>
    Storable (Pi (TKind (KPoly1)) r a)
  where
    sizeOf _ = sizeOf (undefined :: r)
    alignment _ = alignment (undefined :: r)
    peek = fmap (fromJustPi . loadPi (Proxy :: Proxy a)) . peek . castPtr
    poke ptr = poke (castPtr ptr) . fromPi


------------------------------------------------------------------------------
instance (Num r, LoadPi (TKind (KPoly1)) r a) => Num (Pi (TKind (KPoly1)) r a)
  where
    (+) = lift2 (Proxy :: Proxy a) (+)
    (*) = lift2 (Proxy :: Proxy a) (*)
    (-) = lift2 (Proxy :: Proxy a) (-)
    negate = lift1 (Proxy :: Proxy a) negate
    abs = lift1 (Proxy :: Proxy a) abs
    signum = lift1 (Proxy :: Proxy a) signum
    fromInteger = lift0 (Proxy :: Proxy a) . fromInteger


------------------------------------------------------------------------------
instance (Real r, LoadPi (TKind (KPoly1)) r a) =>
    Real (Pi (TKind (KPoly1)) r a)
  where
    toRational = toRational . fromPi


------------------------------------------------------------------------------
instance (Integral r, LoadPi (TKind (KPoly1)) r a) =>
    Integral (Pi (TKind (KPoly1)) r a)
  where
    quot = lift2 (Proxy :: Proxy a) quot
    rem = lift2 (Proxy :: Proxy a) rem
    div = lift2 (Proxy :: Proxy a) div
    mod = lift2 (Proxy :: Proxy a) mod
    quotRem a b = lift0 (Proxy :: Proxy a) *** lift0 (Proxy :: Proxy a) $
        quotRem (fromPi a) (fromPi b)
    divMod a b = lift0 (Proxy :: Proxy a) *** lift0 (Proxy :: Proxy a) $
        divMod (fromPi a) (fromPi b)
    toInteger = toInteger . fromPi


------------------------------------------------------------------------------
instance (Integral r, Known a, Val a ~ r) =>
    Integral (Pi (TKind (KPoly1)) r (Just a))
  where
    quot = const
    rem = const
    div = const
    mod = const
    quotRem = (,)
    divMod = (,)
    toInteger = toInteger . fromPi


------------------------------------------------------------------------------
instance (Fractional r, LoadPi (TKind (KPoly1)) r a) =>
    Fractional (Pi (TKind (KPoly1)) r a)
  where
    (/) = lift2 (Proxy :: Proxy a) (/)
    recip = lift1 (Proxy :: Proxy a) recip
    fromRational = lift0 (Proxy :: Proxy a) . fromRational


------------------------------------------------------------------------------
instance (RealFrac r, LoadPi (TKind (KPoly1)) r a) =>
    RealFrac (Pi (TKind (KPoly1)) r a)
  where
    properFraction x = (a, lift0 (Proxy :: Proxy a) b)
      where
        (a, b) = properFraction (fromPi x)
    truncate = truncate . fromPi
    round = round . fromPi
    ceiling = ceiling . fromPi
    floor = floor . fromPi


------------------------------------------------------------------------------
instance (Floating r, LoadPi (TKind (KPoly1)) r a) =>
    Floating (Pi (TKind (KPoly1)) r a)
  where
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
instance RealFloat r => RealFloat (Pi (TKind (KPoly1)) r Nothing) where
    floatRadix = floatRadix . fromPi
    floatDigits = floatDigits . fromPi
    floatRange = floatRange . fromPi
    decodeFloat = decodeFloat . fromPi
    encodeFloat m n = lift0 (Proxy :: Proxy a) (encodeFloat m n)
    exponent = exponent . fromPi
    significand = lift1 (Proxy :: Proxy a) significand
    scaleFloat n = lift1 (Proxy :: Proxy a) (scaleFloat n)
    isNaN = isNaN . fromPi
    isInfinite = isInfinite . fromPi
    isDenormalized = isDenormalized . fromPi
    isNegativeZero = isNegativeZero . fromPi
    isIEEE = isIEEE . fromPi
    atan2 = lift2 (Proxy :: Proxy a) atan2


------------------------------------------------------------------------------
instance (Bits r, LoadPi (TKind (KPoly1)) r a) =>
    Bits (Pi (TKind (KPoly1)) r a)
  where
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
    testBit a i = testBit (fromPi a) i
    isSigned = isSigned . fromPi
    bitSize = bitSize . fromPi
#if MIN_VERSION_base(4, 5, 0)
    unsafeShiftL a i = lift1 (Proxy :: Proxy a) (flip unsafeShiftL i) a
    unsafeShiftR a i = lift1 (Proxy :: Proxy a) (flip unsafeShiftR i) a
    popCount = popCount . fromPi
#endif
#if MIN_VERSION_base(4, 7, 0)
    bitSizeMaybe = bitSizeMaybe . fromPi
    zeroBits = lift0 (Proxy :: Proxy a) zeroBits


------------------------------------------------------------------------------
instance (FiniteBits r, LoadPi (TKind (KPoly1)) r a) =>
    FiniteBits (Pi (TKind (KPoly1)) r a)
  where
    finiteBitSize = finiteBitSize . fromPi
#if MIN_VERSION_base(4, 8, 0)
    countLeadingZeros = countLeadingZeros . fromPi
    countTrailingZeros = countTrailingZeros . fromPi
#endif
#endif


------------------------------------------------------------------------------
instance (IsString r, LoadPi (TKind (KPoly1)) r a) =>
    IsString (Pi (TKind (KPoly1)) r a)
  where
    fromString = fromJustPi . loadPi (Proxy :: Proxy a) . fromString


------------------------------------------------------------------------------
instance NFData r => NFData (Pi (TKind (KPoly1)) r a) where
    rnf = rnf . fromPi
#ifdef GenericDeriving


------------------------------------------------------------------------------
#if MIN_VERSION_base(4, 9, 0)
type PiD1 = 'MetaData "Pi" "Data.Pi" "types" 'False
type PiC1 = 'MetaCons "Pi" 'PrefixI 'False
type PiS1 =
    'MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy
#else
data PiD1
data PiC1
type PiS1 = NoSelector


------------------------------------------------------------------------------
instance Datatype PiD1 where
    datatypeName _ = "Pi"
    moduleName _ = "Data.Pi"


------------------------------------------------------------------------------
instance Constructor PiC1 where
    conName _ = "Pi"
#endif


------------------------------------------------------------------------------
instance LoadPi (TKind (KPoly1)) r a => Generic (Pi (TKind (KPoly1)) r a)
  where
    type Rep (Pi (TKind (KPoly1)) r a) = D1 PiD1 (C1 PiC1 (S1 PiS1 (Rec0 r)))
    from = M1 . M1 . M1 . K1 . fromPi
    to (M1 (M1 (M1 (K1 a)))) = lift0 (Proxy :: Proxy a) a
#endif


------------------------------------------------------------------------------
instance Eq r => TestEquality (Pi (TKind (KPoly1)) r) where
    testEquality (Pi a) (Pi b)
        | val a == val b = Just (unsafeCoerce Refl)
        | otherwise = Nothing
    testEquality (Some _) (Some _) = Just Refl
    testEquality _ _ = Nothing


------------------------------------------------------------------------------
upcast :: Pi (TKind (KPoly1)) r (Just a) -> Pi (TKind (KPoly1)) r Nothing
upcast (Pi p) = Some p
#if __GLASGOW_HASKELL__ < 704
upcast _ = undefined
#endif


------------------------------------------------------------------------------
topcast :: Pi (TKind (KPoly1)) r a -> Pi (TKind (KPoly1)) r Nothing
topcast (Pi p) = Some p
topcast (Some p) = Some p


------------------------------------------------------------------------------
downcast
    :: Pi (TKind (KPoly1)) r Nothing
    -> (forall b. (Known b, Val b ~ r) => Pi (TKind (KPoly1)) r (Just b) -> c)
    -> c
downcast (Some p) f = f (Pi p)
#if __GLASGOW_HASKELL__ < 704
downcast _ _ = undefined
#endif


------------------------------------------------------------------------------
bottomcast
    :: Pi (TKind (KPoly1)) r a
    -> (forall b. (Known b, Val b ~ r) => Pi (TKind (KPoly1)) r (Just b) -> c)
    -> c
bottomcast (Pi p) f = f (Pi p)
bottomcast (Some p) f = f (Pi p)
