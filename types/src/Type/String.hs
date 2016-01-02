{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
#endif

#include "kinds.h"

module Type.String
    ( String
#ifdef DataPolyKinds
    , Symbol
#endif
    )
where

-- base ----------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 708 && defined(DataPolyKinds)
import           Data.Type.Equality (type (==))
#endif
import           Data.Typeable (Typeable)
import qualified Prelude as S (String)


-- types ---------------------------------------------------------------------
#ifdef DataPolyKinds
import           Type.Char (KChar)
#endif
import           Type.Eq ((:==))
import           Type.Meta (Known, val, Proxy (Proxy))
import           Type.Ord (Compare)
import           Type.Semigroup ((:<>))


------------------------------------------------------------------------------
#ifdef DataPolyKinds
type String = 'String


------------------------------------------------------------------------------
data Symbol = String (KList (KChar))
  deriving (Typeable)
#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable String
#endif
#else
data String (ns :: KList (KChar))
  deriving (Typeable)
#endif


------------------------------------------------------------------------------
instance Known S.String cs => Known S.String (String cs) where
    val _ = val (Proxy :: Proxy cs)
    {-# INLINE val #-}


#if __GLASGOW_HASKELL__ >= 708 && defined(DataPolyKinds)
------------------------------------------------------------------------------
type instance (as :: KString) == (bs :: KString) = as :== bs


#endif
------------------------------------------------------------------------------
type instance String as :== String bs = as :== bs


------------------------------------------------------------------------------
type instance Compare (String as) (String bs) = Compare as bs


------------------------------------------------------------------------------
type instance String as :<> String bs = String (as :<> bs)