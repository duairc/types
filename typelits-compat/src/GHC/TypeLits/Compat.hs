{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}

#if __GLASGOW_HASKELL__ >= 702
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
#endif

#if __GLASGOW_HASKELL__ >= 706 && __GLASGOW_HASKELL__ < 708
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
#endif

module GHC.TypeLits.Compat
    ( (:+)
    , (:*)
    , (:^)
    , (:-)
    , KnownNat
    , natVal
    , KnownSymbol
    , symbolVal
    )
where

#if __GLASGOW_HASKELL__ >= 706
-- base ----------------------------------------------------------------------
import           GHC.TypeLits
#if __GLASGOW_HASKELL__ >= 708
                     ( type (+)
                     , type (*)
                     , type (^)
                     , type (-)
                     , KnownNat
                     , natVal
                     , KnownSymbol
                     , symbolVal
                     )
#else
                     ( Sing
                     , SingI
                     , SingRep
                     , Symbol
                     , fromSing
                     , sing
                     )

#endif
#endif
#if __GLASGOW_HASKELL__ < 708
-- typelits-compat -----------------------------------------------------------
import           Type.Nat.Compat ((:+), (:*), (:^), (:-), KnownNat, natVal)
#if __GLASGOW_HASKELL__ < 706
import           Type.Symbol.Compat (KnownSymbol, symbolVal)
#endif
#endif
#if __GLASGOW_HASKELL__ >= 706 && __GLASGOW_HASKELL__ < 708


------------------------------------------------------------------------------
class SingI n => KnownSymbol (n :: Symbol) where
    symbolSing :: Sing n


------------------------------------------------------------------------------
instance SingRep n String => KnownSymbol n where
    symbolSing = sing


------------------------------------------------------------------------------
symbolVal :: forall n proxy. KnownSymbol n => proxy n -> String
symbolVal _ = fromSing (symbolSing :: Sing n)
#endif
#if __GLASGOW_HASKELL__ >= 708


------------------------------------------------------------------------------
type a :+ b = a + b


------------------------------------------------------------------------------
type a :* b = a * b


------------------------------------------------------------------------------
type a :^ b = a ^ b


------------------------------------------------------------------------------
type a :- b = a - b
#endif