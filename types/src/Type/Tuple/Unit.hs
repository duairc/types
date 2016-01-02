{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Safe #-}
#endif

#include "kinds.h"

module Type.Tuple.Unit
    ( Unit
    )
where

#ifndef DataPolyKinds
-- base ----------------------------------------------------------------------
import           Data.Typeable (Typeable)


-- types ---------------------------------------------------------------------
import {-# SOURCE #-} Type.Bool (True)
import           Type.Eq ((:==))
import           Type.Meta (Known, val)
import           Type.Ord (Compare)
import           Type.Ordering (EQ)
import           Type.Semigroup ((:<>))


#endif
------------------------------------------------------------------------------
#ifdef DataPolyKinds
type Unit = '()
#else
data Unit
  deriving (Typeable)


------------------------------------------------------------------------------
instance Known () Unit where
    val _ = ()


------------------------------------------------------------------------------
type instance Unit :== Unit = True


------------------------------------------------------------------------------
type instance Compare Unit Unit = EQ


------------------------------------------------------------------------------
type instance Unit :<> Unit = Unit
#endif