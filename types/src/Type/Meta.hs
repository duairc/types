{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

module Type.Meta
    ( Known, Val, val, same
    )
where

-- base ----------------------------------------------------------------------
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
import           Unsafe.Coerce (unsafeCoerce)


-- types ---------------------------------------------------------------------
import           Type.Meta.Equality ((:~:) (Refl))
#ifdef DataPolyKinds
import           Type.Meta.Proxy (Proxy (Proxy))
import           Type.Meta.Void (Void)
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


------------------------------------------------------------------------------
same :: (Eq (Val a), Val a ~ Val b, Known a, Known b)
    => proxy a
    -> proxy' b
    -> Maybe (a :~: b)
same a b = if val a == val b then Just (unsafeCoerce Refl) else Nothing
