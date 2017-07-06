{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "kinds.h"

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

module Type.Meta.Hashable
    ()
where

-- hashable ------------------------------------------------------------------
import           Data.Hashable
                     ( Hashable, hashWithSalt
#if !MIN_VERSION_base(4, 7, 0)
                     , hash
#endif
                     )
import           Data.Hashable.Lifted (Hashable1, liftHashWithSalt)


-- types ---------------------------------------------------------------------
import           Data.Pi (Pi, fromPi)
import           Data.Sing (Sing, fromSing, Some, fromSome)
#if !MIN_VERSION_base(4, 7, 0)
import           Type.Meta.Proxy (Proxy)
#endif
#if defined(DataPolyKinds) && !defined(KindsAreTypes)
import           Type.Meta.Proxy (KProxy)
#endif
#if !MIN_VERSION_base(4, 8, 0)
import           Type.Meta.Void (Void, absurd)
#endif


------------------------------------------------------------------------------
instance Hashable r => Hashable (Pi (TKind (KPoly1)) r a) where
    hashWithSalt s = hashWithSalt s . fromPi


------------------------------------------------------------------------------
instance Hashable r => Hashable (Sing r a) where
    hashWithSalt s = hashWithSalt s . fromSing


------------------------------------------------------------------------------
instance Hashable r => Hashable (Some (TKind (KPoly1)) r) where
    hashWithSalt s = hashWithSalt s . fromSome


------------------------------------------------------------------------------
instance Hashable1 (Some (TKind (KPoly1))) where
    liftHashWithSalt hws s = hws s . fromSome
#if !MIN_VERSION_base(4, 7, 0)


------------------------------------------------------------------------------
instance Hashable (Proxy a) where
    hash _ = 0
    hashWithSalt s _ = s


------------------------------------------------------------------------------
instance Hashable1 Proxy where
    liftHashWithSalt _ s _ = s
#endif
#if !MIN_VERSION_base(4, 8, 0)


------------------------------------------------------------------------------
instance Hashable Void where
    hashWithSalt _ = absurd
#endif
