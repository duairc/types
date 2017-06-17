{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import           Type.Meta
                     ( Sing (Sing), Some (Some), val
#if !MIN_VERSION_base(4, 7, 0)
                     , Proxy
#endif
#if !MIN_VERSION_base(4, 8, 0)
                     , Void, absurd
#endif
                     )


------------------------------------------------------------------------------
instance Hashable r => Hashable (Sing r a) where
    hashWithSalt s (Sing a) = hashWithSalt s (val a)


------------------------------------------------------------------------------
instance Hashable r => Hashable (Some r) where
    hashWithSalt s (Some (Sing a)) = hashWithSalt s (val a)


------------------------------------------------------------------------------
instance Hashable1 Some where
    liftHashWithSalt hws s (Some (Sing a)) = hws s (val a)
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
    hashWithSalt s = absurd
#endif
