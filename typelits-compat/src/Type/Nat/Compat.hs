{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif

module Type.Nat.Compat
    ( Z, S
    , (:+)
    , (:*)
    , (:^)
    , (:-)
    , KnownNat
    , natVal
    )
where


------------------------------------------------------------------------------
data Z


------------------------------------------------------------------------------
data S n


------------------------------------------------------------------------------
type family m :+ n
type instance m :+ Z = m
type instance m :+ (S n) = S (m :+ n)


------------------------------------------------------------------------------
type family m :* n
type instance m :* Z = Z
type instance m :* S n = m :+ (m :* n)


------------------------------------------------------------------------------
type family m :^ n
type instance m :^ Z = S Z
type instance m :^ S n = (m :^ n) :* m


------------------------------------------------------------------------------
type family m :- n
type instance m :- Z = m
type instance (S m) :- (S n) = m :- n


------------------------------------------------------------------------------
class KnownNat n where
    natVal :: proxy n -> Integer


------------------------------------------------------------------------------
instance KnownNat Z where
    natVal _ = 0


------------------------------------------------------------------------------
instance KnownNat n => KnownNat (S n) where
    natVal _ = 1 + natVal (Proxy :: Proxy n)


------------------------------------------------------------------------------
data Proxy a = Proxy