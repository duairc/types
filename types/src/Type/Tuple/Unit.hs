{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
{-# LANGUAGE Safe #-}
#endif

module Type.Tuple.Unit
    ( Unit
    )
where

#ifndef DataPolyKinds
-- base ----------------------------------------------------------------------
import           Data.Typeable (Typeable)


-- types ---------------------------------------------------------------------
import           Type.Bool (True)
import           Type.Eq ((:==))
import           Type.Meta (Known, Val, val)
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
instance Known Unit where
    type Val Unit = ()
    val _ = ()


------------------------------------------------------------------------------
type instance Unit :== Unit = True


------------------------------------------------------------------------------
type instance Compare Unit Unit = EQ


------------------------------------------------------------------------------
type instance Unit :<> Unit = Unit
#endif
