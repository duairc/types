{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}

#include "kinds.h"

#ifdef GenericDeriving
{-# LANGUAGE DeriveGeneric #-}
#endif

#ifdef SafeHaskell
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
#endif

module Type.Meta.Void
    ( Void, absurd
    )
where

-- base ----------------------------------------------------------------------
#if MIN_VERSION_base(4, 8, 0)
import           Data.Void (Void, absurd)
#else
import           Control.Exception (Exception)
import           Data.Ix (Ix, range, index, inRange, rangeSize)
import           Data.Typeable (Typeable)
#if defined(GenericDeriving)
import           GHC.Generics (Generic)
#endif


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData, rnf)


------------------------------------------------------------------------------
data Void
  deriving
    ( Typeable
#ifdef GenericDeriving
    , Generic
#endif
    )


------------------------------------------------------------------------------
instance Eq Void where
    _ == _ = True


------------------------------------------------------------------------------
instance Ord Void where
    compare _ _ = EQ


------------------------------------------------------------------------------
instance Ix Void where
    range _     = []
    index _     = absurd
    inRange _   = absurd
    rangeSize _ = 0


------------------------------------------------------------------------------
instance Read Void where
    readsPrec _ _ = []


------------------------------------------------------------------------------
instance Show Void where
    showsPrec _ = absurd


------------------------------------------------------------------------------
instance Exception Void


------------------------------------------------------------------------------
instance NFData Void where
    rnf = absurd


------------------------------------------------------------------------------
absurd :: Void -> a
absurd _ = undefined
#endif
