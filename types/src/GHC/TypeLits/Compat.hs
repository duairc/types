{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#include "kinds.h"

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
#if defined(UseTypeLits) && MIN_VERSION_base(4, 8, 0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
#endif

module GHC.TypeLits.Compat
    (
#ifdef DataPolyKinds
    -- * Kinds
      Nat
    , Symbol
    ,
#endif
    -- * Linking type and value level
      KnownNat
    , natVal
    , KnownSymbol
    , symbolVal
    , SomeNat (SomeNat)
    , SomeSymbol (SomeSymbol)
    , someNatVal
    , someSymbolVal
    , sameNat
    , sameSymbol
    -- * Functions on type literals
    , (:<=)
    , (:<=?)
    , (:+)
    , (:-)
    , (:*)
    , (:^)
    , CmpNat
    , CmpSymbol
    -- * Values
    , Zero
    , One
    )
where

-- base ----------------------------------------------------------------------
#ifdef UseTypeLits
import           GHC.TypeLits
                     ( Nat, KnownNat, natVal
                     , Symbol, KnownSymbol, symbolVal
                     , SomeNat (SomeNat), someNatVal, sameNat
                     , SomeSymbol (SomeSymbol), someSymbolVal, sameSymbol
                     , type (<=)
                     , type (<=?)
                     , type (+)
                     , type (-)
                     , type (*)
                     , type (^)
                     , CmpNat
                     , CmpSymbol
                     )


------------------------------------------------------------------------------
class (<=) a b => (:<=) a b


------------------------------------------------------------------------------
instance (<=) a b => (:<=) a b


------------------------------------------------------------------------------
type a :<=? b = a <=? b


------------------------------------------------------------------------------
type a :+ b = a + b


------------------------------------------------------------------------------
type a :- b = a - b


------------------------------------------------------------------------------
type a :* b = a * b


------------------------------------------------------------------------------
type a :^ b = a ^ b


------------------------------------------------------------------------------
type Zero = 0


------------------------------------------------------------------------------
type One = 1
#else
import           Data.Typeable (Typeable)
import           GHC.Exts (Any)
import           Unsafe.Coerce (unsafeCoerce)


-- types ---------------------------------------------------------------------
import           Type.Bool (True)
import           Type.Meta (Known, Val, val, Proxy (Proxy), (:~:) (Refl))
import           Type.Natural
                     (
#ifdef DataPolyKinds
                       Nat
                     ,
#endif
                       One
                     , Zero
                     )
import qualified Type.Num as N ((:+), (:-), (:*), (:^))
import           Type.Ord (Compare)
import qualified Type.Ord as O ((:<=))
#ifdef DataPolyKinds
import           Type.String (Symbol)
#endif


------------------------------------------------------------------------------
class KnownNat (a :: KNatural) where
    natVal :: proxy a -> Integer


------------------------------------------------------------------------------
instance (Known a, Val a ~ Integer) => KnownNat a where
    natVal = toInteger . val
    {-# INLINE natVal #-}


------------------------------------------------------------------------------
class KnownSymbol (a :: KString) where
    symbolVal :: proxy a -> String


------------------------------------------------------------------------------
instance (Known a, Val a ~ String) => KnownSymbol a where
    symbolVal = val
    {-# INLINE symbolVal #-}


------------------------------------------------------------------------------
data SomeNat = forall n. KnownNat n => SomeNat (Proxy n)
  deriving (Typeable)


------------------------------------------------------------------------------
instance Eq SomeNat where
    SomeNat x == SomeNat y = natVal x == natVal y


------------------------------------------------------------------------------
instance Ord SomeNat where
    compare (SomeNat x) (SomeNat y) = compare (natVal x) (natVal y)


------------------------------------------------------------------------------
instance Show SomeNat where
    showsPrec p (SomeNat x) = showsPrec p (natVal x)


------------------------------------------------------------------------------
instance Read SomeNat where
    readsPrec p xs = do
        (a, ys) <- readsPrec p xs
        case someNatVal a of
            Nothing -> []
            Just n -> [(n, ys)]


------------------------------------------------------------------------------
data SomeSymbol = forall n. KnownSymbol n => SomeSymbol (Proxy n)
  deriving (Typeable)


------------------------------------------------------------------------------
instance Eq SomeSymbol where
    SomeSymbol x == SomeSymbol y = symbolVal x == symbolVal y


------------------------------------------------------------------------------
instance Ord SomeSymbol where
    SomeSymbol x `compare` SomeSymbol y = symbolVal x `compare` symbolVal y


------------------------------------------------------------------------------
instance Show SomeSymbol where
    showsPrec p (SomeSymbol x) = showsPrec p (symbolVal x)


------------------------------------------------------------------------------
instance Read SomeSymbol where
    readsPrec p xs = do
        (a, ys) <- readsPrec p xs
        return (someSymbolVal a, ys)


------------------------------------------------------------------------------
someNatVal :: Integer -> Maybe SomeNat
someNatVal n
    | n < 0 = Nothing
    | otherwise = Just $ withNatVal p n SomeNat p
  where
    p = Proxy :: Proxy Any


------------------------------------------------------------------------------
someSymbolVal :: String -> SomeSymbol
someSymbolVal n = withSymbolVal p n SomeSymbol p
  where
    p = Proxy :: Proxy Any


------------------------------------------------------------------------------
sameNat :: (KnownNat a, KnownNat b) => Proxy a -> Proxy b -> Maybe (a :~: b)
sameNat x y
    | natVal x == natVal y = Just (unsafeCoerce Refl)
    | otherwise = Nothing


------------------------------------------------------------------------------
sameSymbol
    :: (KnownSymbol a, KnownSymbol b) => Proxy a -> Proxy b -> Maybe (a :~: b)
sameSymbol x y
    | symbolVal x == symbolVal y = Just (unsafeCoerce Refl)
    | otherwise = Nothing


------------------------------------------------------------------------------
newtype NatVal n a = NatVal (KnownNat n => a)


------------------------------------------------------------------------------
newtype SymbolVal n a = SymbolVal (KnownSymbol n => a)


------------------------------------------------------------------------------
withNatVal :: forall a n proxy. proxy n -> Integer -> (KnownNat n => a) -> a
withNatVal _ n f = unsafeCoerce (NatVal f :: NatVal n a) (const n)


------------------------------------------------------------------------------
withSymbolVal
    :: forall a n proxy. proxy n -> String -> (KnownSymbol n => a) -> a
withSymbolVal _ n f = unsafeCoerce (SymbolVal f :: SymbolVal n a) (const n)


------------------------------------------------------------------------------
class
#ifdef EqualitySuperclassConstraints
    (a :<=? b) ~ True =>
#endif
    a :<= b


------------------------------------------------------------------------------
instance (a :<=? b) ~ True => (:<=) a b


------------------------------------------------------------------------------
type (a :: KNatural) :<=? (b :: KNatural) = a O.:<= b


------------------------------------------------------------------------------
type (a :: KNatural) :+ (b :: KNatural) = a N.:+ b


------------------------------------------------------------------------------
type (a :: KNatural) :- (b :: KNatural) = a N.:- b


------------------------------------------------------------------------------
type (a :: KNatural) :* (b :: KNatural) = a N.:* b


------------------------------------------------------------------------------
type (a :: KNatural) :^ (b :: KNatural) = a N.:^ b


------------------------------------------------------------------------------
type CmpNat (a :: KNatural) (b :: KNatural) = Compare a b


------------------------------------------------------------------------------
type CmpSymbol (a :: KString) (b :: KString) = Compare a b
#endif
