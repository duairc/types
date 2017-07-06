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

#include "kinds.h"

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
#if MIN_VERSION_base(4, 7, 0) && !MIN_VERSION_base(4, 9, 0)
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
#endif

module Type.String
    ( String
#ifdef DataPolyKinds
    , Symbol
#endif
    )
where

-- base ----------------------------------------------------------------------
#if MIN_VERSION_base(4, 7, 0) && defined(DataPolyKinds)
import           Data.Type.Equality (type (==))
#endif
import           Data.Typeable (Typeable)
import qualified Prelude as S (String)


-- types ---------------------------------------------------------------------
#ifdef DataPolyKinds
import           Type.Char (KChar)
#endif
import           Type.Eq ((:==))
import           Type.Meta (Known, Val, val)
import           Type.Meta.Proxy (Proxy (Proxy))
import           Type.Ord (Compare)
import           Type.Semigroup ((:<>))


------------------------------------------------------------------------------
#ifdef DataPolyKinds
type String = 'String


------------------------------------------------------------------------------
data Symbol = String (KList (KChar))
  deriving (Typeable)
#ifdef PolyTypeable
deriving instance Typeable String
#endif
#else
data String (ns :: KList (KChar))
  deriving (Typeable)
#endif


------------------------------------------------------------------------------
instance (Known cs, Val cs ~ S.String) => Known (String cs) where
    type Val (String cs) = S.String
    val _ = val (Proxy :: Proxy cs)
    {-# INLINE val #-}


#if MIN_VERSION_base(4, 7, 0) && defined(DataPolyKinds)
------------------------------------------------------------------------------
type instance (as :: KString) == (bs :: KString) = as :== bs


#endif
------------------------------------------------------------------------------
type instance String as :== String bs = as :== bs


------------------------------------------------------------------------------
type instance Compare (String as) (String bs) = Compare as bs


------------------------------------------------------------------------------
type instance String as :<> String bs = String (as :<> bs)
