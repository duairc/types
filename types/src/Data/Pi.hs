{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

#include "kinds.h"

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

#if MIN_VERSION_base(4, 7, 0)
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 704
{-# OPTIONS_GHC -O0 #-}
#endif

module Data.Pi
    ( Pi (Pi, SomePi), fromPi, toPi
    )
where

-- base ----------------------------------------------------------------------
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative, pure, (<*>))
#endif
import           Control.Applicative (liftA2)
import           Control.Arrow ((***), first)
import           Control.Monad (guard)
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
import           Data.String (IsString, fromString)
import           Data.Ix
                     ( Ix
                     , range
                     , index
                     , inRange
                     , rangeSize
                     )
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid, mappend, mempty)
#endif
#if MIN_VERSION_base(4, 9, 0)
import           Data.Semigroup (Semigroup, (<>))
#endif
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Traversable (Traversable, traverse)
#endif
#if !defined(DataPolyKinds) || defined(PolyTypeable)
import           Data.Typeable (Typeable)
#endif
import           Foreign.Ptr (castPtr)
import           Foreign.Storable (Storable, alignment, peek, poke, sizeOf)
#if defined(GenericDeriving)
import           GHC.Generics
                     ( Generic, Rep, to, from, Generic1, Rep1, from1, to1
                     , C1, D1, K1 (K1), M1 (M1), S1, Rec0, Par1 (Par1)
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


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData, rnf)


-- types ---------------------------------------------------------------------
import           Type.Maybe (Just, Nothing)
import           Type.Meta
                     ( Known, Val, val
                     , Proxy (Proxy), Sing (Sing), Some (Some), someVal
                     )


------------------------------------------------------------------------------
data Pi a r where
    Pi :: (Known a, Val a ~ r) => !(Proxy a) -> Pi (Just a) r
    SomePi :: (Known b, Val b ~ r) => !(Proxy b) -> Pi Nothing r
#if !defined(DataPolyKinds) || defined(PolyTypeable)
  deriving (Typeable)
#endif


------------------------------------------------------------------------------
fromPi :: Pi a r -> r
fromPi (Pi p) = val p
fromPi (SomePi p) = val p


------------------------------------------------------------------------------
toPi :: a ~ Nothing => r -> Pi a r
toPi r = case someVal r of Some (Sing p) -> SomePi p


------------------------------------------------------------------------------
instance a ~ Nothing => Functor (Pi a) where
    fmap f = pure . f . fromPi


------------------------------------------------------------------------------
instance a ~ Nothing => Applicative (Pi a) where
    pure = toPi
    f <*> a = pure (fromPi f (fromPi a))


------------------------------------------------------------------------------
instance a ~ Nothing => Monad (Pi a) where
    return = pure
    a >>= f = f (fromPi a)


------------------------------------------------------------------------------
instance a ~ Nothing => Foldable (Pi a) where
    foldr f b a = f (fromPi a) b


------------------------------------------------------------------------------
instance a ~ Nothing => Traversable (Pi a) where
    traverse f = fmap pure . f . fromPi


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance Eq1 (Pi a) where
    liftEq _ (Pi _) (Pi _) = True
    liftEq eq (SomePi a) (SomePi b) = eq (val a) (val b)


------------------------------------------------------------------------------
instance Ord1 (Pi a) where
    liftCompare _ (Pi _) (Pi _) = EQ
    liftCompare cmp (SomePi a) (SomePi b) = cmp (val a) (val b)


------------------------------------------------------------------------------
instance a ~ Nothing => Read1 (Pi a) where
    liftReadList _ rl = map (first (map toPi)) . rl
    liftReadsPrec rp _ p = map (first toPi) . rp p


------------------------------------------------------------------------------
instance Show1 (Pi a) where
    liftShowList _ sl = sl . map fromPi
    liftShowsPrec sp _ p = sp p . fromPi


#endif
------------------------------------------------------------------------------
instance Eq r => Eq (Pi a r) where
    Pi _ == Pi _ = True
    SomePi a == SomePi b = val a == val b
#if __GLASGOW_HASKELL__ < 800
    _ == _ = undefined
#endif


------------------------------------------------------------------------------
instance Ord r => Ord (Pi a r) where
    compare (Pi _) (Pi _) = EQ
    compare (SomePi a) (SomePi b) = compare (val a) (val b)
#if __GLASGOW_HASKELL__ < 800
    compare _ _ = undefined
#endif


------------------------------------------------------------------------------
instance Read r => Read (Pi Nothing r) where
    readList s = fmap (first (map toPi)) (readList s)
    readsPrec p s = fmap (first toPi) (readsPrec p s)


------------------------------------------------------------------------------
instance (Known a, r ~ Val a, Read r, Eq r) => Read (Pi (Just a) r) where
    readList s = do
        (rs, s') <- readList s
        guard $ all (== val (Proxy :: Proxy a)) rs
        pure (map (const (Pi Proxy)) rs, s')
    readsPrec p s = do
        (r, s') <- readsPrec p s
        guard $ r == val (Proxy :: Proxy a)
        pure (Pi Proxy, s')


------------------------------------------------------------------------------
instance Show r => Show (Pi a r) where
    showsPrec p = showsPrec p . fromPi
    showList = showList . map fromPi


------------------------------------------------------------------------------
instance Bounded r => Bounded (Pi Nothing r) where
    minBound = toPi minBound
    maxBound = toPi maxBound


------------------------------------------------------------------------------
instance (Known a, Val a ~ r) => Bounded (Pi (Just a) r) where
    minBound = Pi Proxy
    maxBound = Pi Proxy


------------------------------------------------------------------------------
instance Enum r => Enum (Pi Nothing r) where
    toEnum = pure . toEnum
    fromEnum = fromEnum . fromPi


------------------------------------------------------------------------------
instance (Known a, Val a ~ r) => Enum (Pi (Just a) r) where
    toEnum 0 = Pi Proxy
    toEnum _ = error "Data.Pi.Pi.toEnum: bad argument"
    fromEnum _ = 0


------------------------------------------------------------------------------
instance Ix r => Ix (Pi Nothing r) where
    range (a, b) = map pure (range (fromPi a, fromPi b))
    index (a, b) i = index (fromPi a, fromPi b) (fromPi i)
    inRange (a, b) i = inRange (fromPi a, fromPi b) (fromPi i)


------------------------------------------------------------------------------
instance (Known a, Val a ~ r, Ord r) => Ix (Pi (Just a) r) where
    range _ = [Pi Proxy]
    index _ _ = 0
    inRange _ _ = True
    rangeSize _ = 1


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance Semigroup r => Semigroup (Pi Nothing r) where
    (<>) = liftA2 (<>)


------------------------------------------------------------------------------
instance Semigroup (Pi (Just a) r) where
    a <> _ = a


#endif
------------------------------------------------------------------------------
instance Monoid r => Monoid (Pi Nothing r) where
    mempty = pure mempty
    mappend = liftA2 mappend


------------------------------------------------------------------------------
instance (Known a, Val a ~ r) => Monoid (Pi (Just a) r) where
    mempty = Pi Proxy
    mappend a _ = a


------------------------------------------------------------------------------
instance Storable r => Storable (Pi Nothing r) where
    sizeOf _ = sizeOf (undefined :: r)
    alignment _ = alignment (undefined :: r)
    peek ptr = fmap pure $ peek (castPtr ptr)
    poke ptr = poke (castPtr ptr) . fromPi


------------------------------------------------------------------------------
instance (Known a, Val a ~ r) => Storable (Pi (Just a) r) where
    sizeOf _ = 0
    alignment _ = 1
    peek _ = return (Pi Proxy)
    poke _ _ = return ()


------------------------------------------------------------------------------
instance Num r => Num (Pi Nothing r) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger


------------------------------------------------------------------------------
instance (Eq r, Num r, Known a, Val a ~ r) => Num (Pi (Just a) r) where
    (+) = const
    (*) = const
    (-) = const
    signum = id
    abs = id
    negate = id
    fromInteger a
        | fromInteger a == val (Proxy :: Proxy a) = Pi Proxy
        | otherwise = error $
            "Data.Pi.Pi.fromInteger: " ++ show a ++ " out of range"


------------------------------------------------------------------------------
instance Real r => Real (Pi Nothing r) where
    toRational = toRational . fromPi


------------------------------------------------------------------------------
instance (Real r, Known a, Val a ~ r) => Real (Pi (Just a) r) where
    toRational = toRational . fromPi


------------------------------------------------------------------------------
instance Integral r => Integral (Pi Nothing r) where
    quot = liftA2 quot
    rem = liftA2 rem
    div = liftA2 div
    mod = liftA2 mod
    quotRem a b = pure *** pure $ quotRem (fromPi a) (fromPi b)
    divMod a b = pure *** pure $ divMod (fromPi a) (fromPi b)
    toInteger = toInteger . fromPi


------------------------------------------------------------------------------
instance (Integral r, Known a, Val a ~ r) => Integral (Pi (Just a) r) where
    quot = const
    rem = const
    div = const
    mod = const
    quotRem = (,)
    divMod = (,)
    toInteger = toInteger . fromPi


------------------------------------------------------------------------------
instance Fractional r => Fractional (Pi Nothing r) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = pure . fromRational


------------------------------------------------------------------------------
instance (Fractional r, Eq r, Known a, Val a ~ r) =>
    Fractional (Pi (Just a) r)
  where
    (/) = const
    recip = id
    fromRational a
        | fromRational a == val (Proxy :: Proxy a) = Pi Proxy
        | otherwise = error $
            "Data.Pi.Pi.fromRational: " ++ show a ++ " out of range"


------------------------------------------------------------------------------
instance RealFrac r => RealFrac (Pi Nothing r) where
    properFraction x = (a, pure b)
      where
        (a, b) = properFraction (fromPi x)
    truncate = truncate . fromPi
    round = round . fromPi
    ceiling = ceiling . fromPi
    floor = floor . fromPi


------------------------------------------------------------------------------
instance Floating r => Floating (Pi Nothing r) where
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
instance RealFloat r => RealFloat (Pi Nothing r) where
    floatRadix = floatRadix . fromPi
    floatDigits = floatDigits . fromPi
    floatRange = floatRange . fromPi
    decodeFloat = decodeFloat . fromPi
    encodeFloat m n = pure (encodeFloat m n)
    exponent = exponent . fromPi
    significand = fmap significand
    scaleFloat n = fmap (scaleFloat n)
    isNaN = isNaN . fromPi
    isInfinite = isInfinite . fromPi
    isDenormalized = isDenormalized . fromPi
    isNegativeZero = isNegativeZero . fromPi
    isIEEE = isIEEE . fromPi
    atan2 = liftA2 atan2


------------------------------------------------------------------------------
instance Bits r => Bits (Pi Nothing r) where
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
    testBit a i = testBit (fromPi a) i
    isSigned = isSigned . fromPi
    bitSize = bitSize . fromPi
#if MIN_VERSION_base(4, 5, 0)
    unsafeShiftL a i = fmap (flip unsafeShiftL i) a
    unsafeShiftR a i = fmap (flip unsafeShiftR i) a
    popCount = popCount . fromPi
#endif
#if MIN_VERSION_base(4, 7, 0)
    bitSizeMaybe = bitSizeMaybe . fromPi
    zeroBits = pure zeroBits


------------------------------------------------------------------------------
instance FiniteBits r => FiniteBits (Pi Nothing r) where
    finiteBitSize = finiteBitSize . fromPi
#if MIN_VERSION_base(4, 8, 0)
    countLeadingZeros = countLeadingZeros . fromPi
    countTrailingZeros = countTrailingZeros . fromPi
#endif
#endif


------------------------------------------------------------------------------
instance IsString r => IsString (Pi Nothing r) where
    fromString = pure . fromString


------------------------------------------------------------------------------
instance (IsString r, Eq r, Known a, Val a ~ r) => IsString (Pi (Just a) r)
  where
    fromString a
        | fromString a == val (Proxy :: Proxy a) = Pi Proxy
        | otherwise = error $
            "Data.Pi.Pi.fromString: " ++ show a ++ " wrong value"


------------------------------------------------------------------------------
instance NFData r => NFData (Pi a r) where
    rnf = rnf . fromPi
#ifdef GenericDeriving


------------------------------------------------------------------------------
#if MIN_VERSION_base(4, 9, 0)
type PiD1 = 'MetaData "Pi" "Data.Pi" "types" 'False
type PiC1 = 'MetaCons "Pi" 'PrefixI 'False
type PiC2 = 'MetaCons "SomePi" 'PrefixI 'False
type PiS1 =
    'MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy
#else
data PiD1
data PiC1
data PiC2
type PiS1 = NoSelector


------------------------------------------------------------------------------
instance Datatype PiD1 where
    datatypeName _ = "Pi"
    moduleName _ = "Data.Pi"


------------------------------------------------------------------------------
instance Constructor PiC1 where
    conName _ = "Pi"


------------------------------------------------------------------------------
instance Constructor PiC2 where
    conName _ = "SomePi"
#endif


------------------------------------------------------------------------------
instance Generic (Pi (Nothing :: KMaybe (KPoly1)) r) where
    type Rep (Pi (Nothing :: KMaybe (KPoly1)) r) =
        D1 PiD1 (C1 PiC2 (S1 PiS1 (Rec0 r)))
    from = M1 . M1 . M1 . K1 . fromPi
    to (M1 (M1 (M1 (K1 a)))) = pure a


------------------------------------------------------------------------------
instance a ~ Nothing => Generic1 (Pi a) where
    type Rep1 (Pi a) = D1 PiD1 (C1 PiC2 (S1 PiS1 Par1))
    from1 = M1 . M1 . M1 . Par1 . fromPi
    to1 (M1 (M1 (M1 (Par1 a)))) = pure a


------------------------------------------------------------------------------
instance (Known a, Val a ~ r) => Generic (Pi (Just a) r) where
    type Rep (Pi (Just a) r) = D1 PiD1 (C1 PiC1 (S1 PiS1 (Rec0 r)))
    from = M1 . M1 . M1 . K1 . fromPi
    to (M1 (M1 (M1 (K1 _)))) = Pi Proxy
#endif
