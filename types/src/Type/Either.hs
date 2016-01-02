{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Type.Either
    ( Left
    , Right
    , IsLeft
    , IsRight
    , FromLeft
    , FromRight
    )
where

#ifndef DataPolyKinds
-- base ----------------------------------------------------------------------
import           Data.Typeable (Typeable)


#endif
-- types ---------------------------------------------------------------------
import           Type.Bool (True, False)
#ifndef DataPolyKinds
import           Type.Eq ((:==))
import           Type.Meta (Known, val, Proxy (Proxy), Void)
import           Type.Ord (Compare)
import           Type.Ordering (LT, GT)
import           Type.Semigroup ((:<>))

#endif
#ifdef DataPolyKinds
------------------------------------------------------------------------------
type Left = 'Left


------------------------------------------------------------------------------
type Right = 'Right
#else
------------------------------------------------------------------------------
data Left a
  deriving (Typeable)


------------------------------------------------------------------------------
instance Known r a => Known (Either r Void) (Left a) where
    val _ = Left (val (Proxy :: Proxy a))


------------------------------------------------------------------------------
data Right a
  deriving (Typeable)


------------------------------------------------------------------------------
instance Known r a => Known (Either Void r) (Right a) where
    val _ = Right (val (Proxy :: Proxy a))


------------------------------------------------------------------------------
type instance Left a :== Left b = a :== b
type instance Left _a :== Right _b = False
type instance Right _a :== Left _b = False
type instance Right a :== Right b = a :== b


------------------------------------------------------------------------------
type instance Compare (Left a) (Left b) = Compare a b
type instance Compare (Left _a) (Right _b) = LT
type instance Compare (Right _a) (Left _b) = GT
type instance Compare (Right a) (Right b) = Compare a b


------------------------------------------------------------------------------
type instance Left _a :<> Left b = Left b
type instance Left _a :<> Right b = Right b
type instance Right a :<> Left _b = Right a
type instance Right a :<> Right _b = Right a
#endif


------------------------------------------------------------------------------
type family IsLeft (a :: KEither (KPoly1, KPoly2)) :: KBool
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    IsLeft (Left _a) = True
#ifndef ClosedTypeFamilies
type instance
#endif
    IsLeft (Right _a) = False


------------------------------------------------------------------------------
type family IsRight (a :: KEither (KPoly1, KPoly2)) :: KBool
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    IsRight (Left _a) = False
#ifndef ClosedTypeFamilies
type instance
#endif
    IsRight (Right _a) = True


------------------------------------------------------------------------------
type family FromLeft (a :: KEither (KPoly1, KPoly2)) :: KPoly1
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    FromLeft (Left a) = a


------------------------------------------------------------------------------
type family FromRight (a :: KEither (KPoly1, KPoly2)) :: KPoly2
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    FromRight (Right a) = a