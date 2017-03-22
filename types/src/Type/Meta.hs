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

#if __GLASGOW_HASKELL__ >= 711
{-# LANGUAGE TypeInType #-}
#endif

#if __GLASGOW_HASKELL__ >= 708
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Type.Meta
    ( Known
    , Val
    , val
    , Sing (Sing)
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
import           Control.Applicative (liftA2)
import           Control.Arrow ((***))
#if __GLASGOW_HASKELL__ < 708
import           Control.Category (Category, id, (.))
#endif
#if __GLASGOW_HASKELL__ < 710
import           Control.Exception (Exception)
#endif
import           Control.Monad (guard)
#if __GLASGOW_HASKELL__ >= 800
import           Data.Bifunctor (first)
#endif
import           Data.Bits
                     ( Bits
#if __GLASGOW_HASKELL__ >= 708
                     , FiniteBits
#endif
                     , (.&.)
                     , (.|.)
                     , bit
                     , bitSize
#if __GLASGOW_HASKELL__ >= 708
                     , bitSizeMaybe
#endif
                     , clearBit
                     , complement
                     , complementBit
#if __GLASGOW_HASKELL__ >= 710
                     , countLeadingZeros
                     , countTrailingZeros
#endif
#if __GLASGOW_HASKELL__ >= 708
                     , finiteBitSize
#endif
                     , isSigned
#if __GLASGOW_HASKELL__ >= 704
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
#if __GLASGOW_HASKELL__ >= 704
                     , unsafeShiftL
                     , unsafeShiftR
#endif
                     , xor
#if __GLASGOW_HASKELL__ >= 708
                     , zeroBits
#endif
                     )
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
#if __GLASGOW_HASKELL__ >= 800
import           Data.Functor.Classes
                     ( Eq1
                     , Ord1
                     , Read1
                     , Show1
                     , liftEq
                     , liftCompare
                     , liftReadList
                     , liftReadsPrec
                     , liftShowList
                     , liftShowsPrec
                     )
#endif
import           Data.Ix
                     ( Ix
                     , range
                     , index
                     , inRange
                     , rangeSize
                     )
#if __GLASGOW_HASKELL__ >= 711
import           Data.Kind (Type)
#endif
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (Monoid, mappend, mempty, mconcat)
#endif
#if __GLASGOW_HASKELL__ >= 708
import           Data.Proxy (Proxy (Proxy))
#endif
#if __GLASGOW_HASKELL__ >= 711
import           Data.Semigroup (Semigroup, (<>))
#endif
import           Data.String (IsString, fromString)
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
import           Data.Typeable (Typeable)
#if __GLASGOW_HASKELL__ >= 710
import           Data.Void (Void, absurd)
#endif
import           Foreign.Ptr (castPtr)
import           Foreign.Storable (Storable, alignment, peek, poke, sizeOf)
#if defined(GenericDeriving)
import           GHC.Generics
                     ( Generic, Rep, to, from, Generic1, Rep1, from1, to1
                     , C1, D1, K1 (K1), M1 (M1), S1, U1 (U1), Rec0, Par1 (Par1)
#if __GLASGOW_HASKELL__ >= 800
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
                     , SingRep
                     , fromSing
                     , sing
                     )
import qualified GHC.TypeLits as G
                     ( Sing
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
    val _ = fromSing (sing :: G.Sing a)
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
    val _ = fromSing (sing :: G.Sing a)
#endif
    {-# INLINE val #-}
#endif
#endif


------------------------------------------------------------------------------
data Sing r a = (Known a, Val a ~ r) => Sing !(Proxy a)
#if !defined(DataPolyKinds) || __GLASGOW_HASKELL__ >= 708
  deriving (Typeable)
#endif


------------------------------------------------------------------------------
instance Eq r => TestEquality (Sing r) where
    testEquality (Sing a) (Sing b)
        | val a == val b = Just (unsafeCoerce Refl)
        | otherwise = Nothing


------------------------------------------------------------------------------
instance Eq (Sing r a) where
    _ == _ = True


------------------------------------------------------------------------------
instance Ord (Sing r a) where
    compare _ _ = EQ


------------------------------------------------------------------------------
instance (Eq r, Read r, Known a, Val a ~ r) => Read (Sing r a) where
    readsPrec p s = do
        (r, s') <- readsPrec p s
        guard $ r == val (Proxy :: Proxy a)
        return (Sing Proxy, s')


------------------------------------------------------------------------------
instance Show r => Show (Sing r a) where
    showsPrec p (Sing a) = showsPrec p (val a)


------------------------------------------------------------------------------
instance (Known a, Val a ~ r) => Bounded (Sing r a) where
    minBound = Sing Proxy
    maxBound = Sing Proxy


------------------------------------------------------------------------------
instance (Known a, Val a ~ r) => Enum (Sing r a) where
    toEnum 0 = Sing Proxy
    toEnum _ = error "Type.Meta.Sing.toEnum: bad argument"
    fromEnum _ = 0


------------------------------------------------------------------------------
instance (Known a, Val a ~ r) => Ix (Sing r a) where
    range _ = [Sing Proxy]
    index _ _ = 0
    inRange _ _ = True
    rangeSize _ = 1


#if __GLASGOW_HASKELL__ >= 711
------------------------------------------------------------------------------
instance Semigroup (Sing r a) where
    a <> _ = a


#endif
------------------------------------------------------------------------------
instance (Known a, Val a ~ r) => Monoid (Sing r a) where
    mempty = Sing Proxy
    mappend _ _ = Sing Proxy
    mconcat _ = Sing Proxy


------------------------------------------------------------------------------
instance (Known a, Val a ~ r) => Storable (Sing r a) where
    sizeOf _ = 0
    alignment _ = 1
    peek _ = return (Sing Proxy)
    poke _ _ = return ()


------------------------------------------------------------------------------
instance (Eq r, Num r, Known a, Val a ~ r) => Num (Sing r a) where
    _ + _ = Sing Proxy
    _ * _ = Sing Proxy
    signum _ = Sing Proxy
    abs _ = Sing Proxy
    negate _ = Sing Proxy
    fromInteger a
        | fromInteger a == val (Proxy :: Proxy a) = Sing Proxy
        | otherwise = error $
            "Type.Meta.Sing.fromInteger: " ++ show a ++ " out of range"


------------------------------------------------------------------------------
instance (Real r, Known a, Val a ~ r) => Real (Sing r a) where
    toRational (Sing r) = toRational (val r)


------------------------------------------------------------------------------
instance (Integral r, Known a, Val a ~ r) => Integral (Sing r a) where
    quot _ _ = Sing Proxy
    rem _ _ = Sing Proxy
    div _ _ = Sing Proxy
    mod _ _ = Sing Proxy
    quotRem _ _ = (Sing Proxy, Sing Proxy)
    divMod _ _ = (Sing Proxy, Sing Proxy)
    toInteger (Sing a) = toInteger (val a)


------------------------------------------------------------------------------
instance (Fractional r, Eq r, Known a, Val a ~ r) => Fractional (Sing r a)
  where
    _ / _ = Sing Proxy
    recip _ = Sing Proxy
    fromRational a
        | fromRational a == val (Proxy :: Proxy a) = Sing Proxy
        | otherwise = error $
            "Type.Meta.Sing.fromRational: " ++ show a ++ " out of range"


------------------------------------------------------------------------------
instance (IsString r, Eq r, Known a, Val a ~ r) => IsString (Sing r a) where
    fromString a
        | fromString a == val (Proxy :: Proxy a) = Sing Proxy
        | otherwise = error $
            "Type.Meta.Sing.fromString: " ++ show a ++ " wrong value"


#ifdef GenericDeriving
------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 800
type SingD1 = 'MetaData "Sing" "Type.Meta" "types" 'False
type SingC1 = 'MetaCons "Sing" 'PrefixI 'False
#else
data SingD1
data SingC1


------------------------------------------------------------------------------
instance Datatype SingD1 where
    datatypeName _ = "Sing"
    moduleName _ = "Type.Meta"


------------------------------------------------------------------------------
instance Constructor SingC1 where
    conName _ = "Sing"
#endif


------------------------------------------------------------------------------
instance (Known a, Val a ~ r) => Generic (Sing r a) where
    type Rep (Sing r a) = D1 SingD1 (C1 SingC1 U1)
    from _ = M1 (M1 U1)
    to (M1 (M1 U1)) = Sing Proxy


#endif
------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 711
data Some r = forall (k :: Type) (a :: k). Some !(Sing r (a :: k))
#else
data Some r = forall a. Some !(Sing r a)
#endif
#if !defined(DataPolyKinds) || __GLASGOW_HASKELL__ >= 708
  deriving (Typeable)
#endif


------------------------------------------------------------------------------
instance Functor Some where
    fmap f (Some (Sing a)) = someVal (f (val a))


------------------------------------------------------------------------------
instance Applicative Some where
    pure = someVal
    Some (Sing f) <*> Some (Sing a) = someVal (val f (val a))


------------------------------------------------------------------------------
instance Monad Some where
    return = pure
    Some (Sing a) >>= f = f (val a)


------------------------------------------------------------------------------
instance Foldable Some where
    foldMap f (Some (Sing a)) = f (val a)


------------------------------------------------------------------------------
instance Traversable Some where
    traverse f (Some (Sing a)) = fmap someVal (f (val a))


#if __GLASGOW_HASKELL__ >= 800
------------------------------------------------------------------------------
instance Eq1 Some where
    liftEq eq (Some (Sing a)) (Some (Sing b)) = eq (val a) (val b)


------------------------------------------------------------------------------
instance Ord1 Some where
    liftCompare cmp (Some (Sing a)) (Some (Sing b)) = cmp (val a) (val b)


------------------------------------------------------------------------------
instance Read1 Some where
    liftReadList _ rl = map (first (map pure)) . rl
    liftReadsPrec rp _ p = map (first pure) . rp p


------------------------------------------------------------------------------
instance Show1 Some where
    liftShowList _ sl = sl . map (\(Some (Sing a)) -> val a)
    liftShowsPrec sp _ p = sp p . (\(Some (Sing a)) -> val a)


#endif
------------------------------------------------------------------------------
instance Eq r => Eq (Some r) where
    Some (Sing a) == Some (Sing b) = val a == val b


------------------------------------------------------------------------------
instance Ord r => Ord (Some r) where
    compare (Some (Sing a)) (Some (Sing b)) = compare (val a) (val b)


------------------------------------------------------------------------------
instance Read r => Read (Some r) where
    readsPrec p xs = do
        (a, ys) <- readsPrec p xs
        return (pure a, ys)


------------------------------------------------------------------------------
instance Show r => Show (Some r) where
    showsPrec p (Some (Sing a)) = showsPrec p (val a)


------------------------------------------------------------------------------
instance Bounded r => Bounded (Some r) where
    minBound = pure minBound
    maxBound = pure maxBound


------------------------------------------------------------------------------
instance Enum r => Enum (Some r) where
    toEnum = pure . toEnum
    fromEnum (Some (Sing r)) = fromEnum (val r)


------------------------------------------------------------------------------
instance Ix r => Ix (Some r) where
    range (Some (Sing a), Some (Sing b)) = map pure (range (val a, val b))
    index (Some (Sing a), Some (Sing b)) (Some (Sing i)) =
        index (val a, val b) (val i)
    inRange (Some (Sing a), Some (Sing b)) (Some (Sing i)) =
        inRange (val a, val b) (val i)


#if __GLASGOW_HASKELL__ >= 711
------------------------------------------------------------------------------
instance Semigroup r => Semigroup (Some r) where
    (<>) = liftA2 (<>)


#endif
------------------------------------------------------------------------------
instance Monoid r => Monoid (Some r) where
    mempty = pure mempty
    mappend = liftA2 mappend


------------------------------------------------------------------------------
instance Storable r => Storable (Some r) where
    sizeOf _ = sizeOf (undefined :: r)
    alignment _ = alignment (undefined :: r)
    peek ptr = fmap pure $ peek (castPtr ptr)
    poke ptr (Some (Sing a)) = poke (castPtr ptr) (val a)


------------------------------------------------------------------------------
instance Num r => Num (Some r) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger


------------------------------------------------------------------------------
instance Real r => Real (Some r) where
    toRational (Some (Sing r)) = toRational (val r)


------------------------------------------------------------------------------
instance Integral r => Integral (Some r) where
    quot = liftA2 quot
    rem = liftA2 rem
    div = liftA2 div
    mod = liftA2 mod
    quotRem (Some (Sing a)) (Some (Sing b)) =
        pure *** pure $ quotRem (val a) (val b)
    divMod (Some (Sing a)) (Some (Sing b)) = 
        pure *** pure $ divMod (val a) (val b)
    toInteger (Some (Sing a)) = toInteger (val a)


------------------------------------------------------------------------------
instance Fractional r => Fractional (Some r) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = pure . fromRational


------------------------------------------------------------------------------
instance RealFrac r => RealFrac (Some r) where
    properFraction (Some (Sing x)) = (a, pure b)
      where
        (a, b) = properFraction (val x)
    truncate (Some (Sing a)) = truncate (val a)
    round (Some (Sing a)) = round (val a)
    ceiling (Some (Sing a)) = ceiling (val a)
    floor (Some (Sing a)) = floor (val a)


------------------------------------------------------------------------------
instance Floating r => Floating (Some r) where
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
instance RealFloat r => RealFloat (Some r) where
    floatRadix (Some (Sing a)) = floatRadix (val a)
    floatDigits (Some (Sing a)) = floatDigits (val a)
    floatRange (Some (Sing a)) = floatRange (val a)
    decodeFloat (Some (Sing a)) = decodeFloat (val a)
    encodeFloat m n = pure (encodeFloat m n)
    exponent (Some (Sing a)) = exponent (val a)
    significand = fmap significand
    scaleFloat n = fmap (scaleFloat n)
    isNaN (Some (Sing a)) = isNaN (val a)
    isInfinite (Some (Sing a)) = isInfinite (val a)
    isDenormalized (Some (Sing a)) = isDenormalized (val a)
    isNegativeZero (Some (Sing a)) = isNegativeZero (val a)
    isIEEE (Some (Sing a)) = isIEEE (val a)
    atan2 = liftA2 atan2


------------------------------------------------------------------------------
instance Bits r => Bits (Some r) where
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
    testBit (Some (Sing a)) i = testBit (val a) i
    isSigned (Some (Sing a)) = isSigned (val a)
    bitSize (Some (Sing a)) = bitSize (val a)
#if __GLASGOW_HASKELL__ >= 704
    unsafeShiftL a i = fmap (flip unsafeShiftL i) a
    unsafeShiftR a i = fmap (flip unsafeShiftR i) a
    popCount (Some (Sing a)) = popCount (val a)
#endif
#if __GLASGOW_HASKELL__ >= 708
    bitSizeMaybe (Some (Sing a)) = bitSizeMaybe (val a)
    zeroBits = pure zeroBits


------------------------------------------------------------------------------
instance FiniteBits r => FiniteBits (Some r) where
    finiteBitSize (Some (Sing a)) = finiteBitSize (val a)
#if __GLASGOW_HASKELL__ >= 710
    countLeadingZeros (Some (Sing a)) = countLeadingZeros (val a)
    countTrailingZeros (Some (Sing a)) = countTrailingZeros (val a)
#endif
#endif


------------------------------------------------------------------------------
instance IsString r => IsString (Some r) where
    fromString = someVal . fromString


#ifdef GenericDeriving
------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 800
type SomeD1 = 'MetaData "Some" "Type.Meta" "types" 'False
type SomeC1 = 'MetaCons "Some" 'PrefixI 'False
type SomeS1 =
    'MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy
#else
data SomeD1
data SomeC1
type SomeS1 = NoSelector


------------------------------------------------------------------------------
instance Datatype SomeD1 where
    datatypeName _ = "Some"
    moduleName _ = "Type.Meta"


------------------------------------------------------------------------------
instance Constructor SomeC1 where
    conName _ = "Some"
#endif


------------------------------------------------------------------------------
instance Generic (Some r) where
    type Rep (Some r) = D1 SomeD1 (C1 SomeC1 (S1 SomeS1 (Rec0 r)))
    from (Some (Sing a)) = M1 (M1 (M1 (K1 (val a))))
    to (M1 (M1 (M1 (K1 a)))) = pure a


------------------------------------------------------------------------------
instance Generic1 Some where
    type Rep1 Some = D1 SomeD1 (C1 SomeC1 (S1 SomeS1 Par1))
    from1 (Some (Sing a)) = M1 (M1 (M1 (Par1 (val a))))
    to1 (M1 (M1 (M1 (Par1 a)))) = pure a


#endif
------------------------------------------------------------------------------
someVal :: r -> Some r
someVal r =
#if __GLASGOW_HASKELL__ < 700
    unsafeCoerce $
#endif
    withVal p r (Some . Sing) p
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
