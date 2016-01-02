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

module Type.Maybe
    ( Nothing
    , Just
    , IsNothing
    , IsJust
    , FromJust
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
import           Type.Ordering (LT, EQ, GT)
import           Type.Semigroup ((:<>))

#endif
#ifdef DataPolyKinds
------------------------------------------------------------------------------
type Nothing = 'Nothing


------------------------------------------------------------------------------
type Just = 'Just
#else
------------------------------------------------------------------------------
data Nothing
  deriving (Typeable)


------------------------------------------------------------------------------
instance Known (Maybe Void) Nothing where
    val _ = Nothing


------------------------------------------------------------------------------
data Just x
  deriving (Typeable)


------------------------------------------------------------------------------
instance Known r a => Known (Maybe r) (Just a) where
    val _ = Just (val (Proxy :: Proxy a))


------------------------------------------------------------------------------
type instance Nothing :== Nothing = True
type instance Nothing :== Just _b = False
type instance Just _a :== Nothing = False
type instance Just a :== Just b = a :== b


------------------------------------------------------------------------------
type instance Compare Nothing Nothing = EQ
type instance Compare Nothing (Just _b) = LT
type instance Compare (Just _a) Nothing = GT
type instance Compare (Just a) (Just b) = Compare a b


------------------------------------------------------------------------------
type instance Nothing :<> Nothing = Nothing
type instance Nothing :<> Just b = Just b
type instance Just a :<> Nothing = Just a
type instance Just a :<> Just b = Just (a :<> b)
#endif


------------------------------------------------------------------------------
type family IsNothing (a :: KMaybe (KPoly1)) :: KBool
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    IsNothing Nothing = True
#ifndef ClosedTypeFamilies
type instance
#endif
    IsNothing (Just _a) = False


------------------------------------------------------------------------------
type family IsJust (a :: KMaybe (KPoly1)) :: KBool
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    IsJust Nothing = False
#ifndef ClosedTypeFamilies
type instance
#endif
    IsJust (Just _a) = True


------------------------------------------------------------------------------
type family FromJust (a :: KMaybe (KPoly1)) :: KPoly1
#ifdef ClosedTypeFamilies
  where
#else
type instance
#endif
    FromJust (Just a) = a