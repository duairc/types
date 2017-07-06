{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

#include "kinds.h"

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
#endif

module Type.Meta.Proxy
    ( Proxy (Proxy), KProxy (KProxy)
    )
where

-- base ----------------------------------------------------------------------
#if MIN_VERSION_base(4, 7, 0)
import           Data.Proxy (Proxy (Proxy), KProxy (KProxy))
#else
import           Control.Applicative (Applicative, pure, (<*>))
import           Data.Foldable
                     ( Foldable, foldMap, fold, foldl, foldr, foldl1, foldr1
                     )
import           Data.Ix (Ix, range, index, inRange, rangeSize)
import           Data.Monoid (Monoid, mappend, mempty, mconcat)
import           Data.Traversable
                     ( Traversable, traverse, sequenceA, mapM, sequence
                     )
#if !defined(DataPolyKinds) || defined(PolyTypeable)
import           Data.Typeable (Typeable)
#endif
#if defined(GenericDeriving)
import           GHC.Generics
                     ( Generic, Rep, to, from, Generic1, Rep1, from1, to1
                     , C1, D1, M1 (M1), U1 (U1)
                     , Datatype, Constructor
                     , datatypeName, moduleName, conName
                     )
#endif
import           Prelude hiding (foldl, foldr, foldl1, foldr1, mapM, sequence)


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData, rnf)


------------------------------------------------------------------------------
data Proxy a = Proxy
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Bounded
#if !defined(DataPolyKinds) || defined(PolyTypeable)
    , Typeable
#endif
    )


------------------------------------------------------------------------------
instance Functor Proxy where
    fmap _ _ = Proxy
    {-# INLINE fmap #-}


------------------------------------------------------------------------------
instance Applicative Proxy where
    pure _ = Proxy
    {-# INLINE pure #-}
    _ <*> _ = Proxy
    {-# INLINE (<*>) #-}


------------------------------------------------------------------------------
instance Monad Proxy where
    return _ = Proxy
    {-# INLINE return #-}
    _ >>= _ = Proxy
    {-# INLINE (>>=) #-}


------------------------------------------------------------------------------
instance Foldable Proxy where
    foldMap _ _ = mempty
    {-# INLINE foldMap #-}
    fold _ = mempty
    {-# INLINE fold #-}
    foldl _ z _ = z
    {-# INLINE foldl #-}
    foldr _ z _ = z
    {-# INLINE foldr #-}
    foldl1 _ _ = error "foldl1: Proxy"
    {-# INLINE foldl1 #-}
    foldr1 _ _ = error "foldr1: Proxy"
    {-# INLINE foldr1 #-}


------------------------------------------------------------------------------
instance Traversable Proxy where
    traverse _ _ = pure Proxy
    {-# INLINE traverse #-}
    sequenceA _ = pure Proxy
    {-# INLINE sequenceA #-}
    mapM _ _ = return Proxy
    {-# INLINE mapM #-}
    sequence _ = return Proxy
    {-# INLINE sequence #-}


------------------------------------------------------------------------------
instance Enum (Proxy a) where
    toEnum 0 = Proxy
    toEnum _ = error "Type.Meta.Proxy.toEnum: bad argument"
    fromEnum Proxy = 0


------------------------------------------------------------------------------
instance Ix (Proxy a) where
    range _ = [Proxy]
    index _ _ = 0
    inRange _ _ = True
    rangeSize _ = 1


------------------------------------------------------------------------------
instance Monoid (Proxy a) where
    mempty = Proxy
    mappend _ _ = Proxy
    mconcat _ = Proxy


------------------------------------------------------------------------------
instance NFData (Proxy a) where
    rnf Proxy = ()
#ifdef GenericDeriving


------------------------------------------------------------------------------
data ProxyD1
data ProxyC1


------------------------------------------------------------------------------
instance Datatype ProxyD1 where
    datatypeName _ = "Proxy"
    moduleName _ = "Data.Proxy"


------------------------------------------------------------------------------
instance Constructor ProxyC1 where
    conName _ = "Proxy"


------------------------------------------------------------------------------
instance Generic (Proxy a) where
    type Rep (Proxy a) = D1 ProxyD1 (C1 ProxyC1 U1)
    from _ = M1 (M1 U1)
    to _ = Proxy


------------------------------------------------------------------------------
instance Generic1 Proxy where
    type Rep1 Proxy = D1 ProxyD1 (C1 ProxyC1 U1)
    from1 _ = M1 (M1 U1)
    to1 _ = Proxy
#endif


------------------------------------------------------------------------------
data KProxy (t :: *) = KProxy
#endif
