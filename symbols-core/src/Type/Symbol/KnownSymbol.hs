{-# LANGUAGE CPP #-}

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

module Type.Symbol.KnownSymbol
    ( KnownSymbol
    , symbolVal
    )
where

#if __GLASGOW_HASKELL__ >= 706
-- base ----------------------------------------------------------------------
import           GHC.TypeLits
#if __GLASGOW_HASKELL__ >= 708
                     ( KnownSymbol
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
#else
-- symbols-core --------------------------------------------------------------
import           Type.Symbol.Internal (KnownSymbol, symbolVal)
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