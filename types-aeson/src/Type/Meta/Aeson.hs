{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "kinds.h"

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

module Type.Meta.Aeson
    ()
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson
                     ( FromJSON, parseJSON
                     , ToJSON, toJSON
#if !MIN_VERSION_base(4, 7, 0)
                     , Value (Null)
#endif
                     )
#if MIN_VERSION_aeson(1, 0, 0)
import           Data.Aeson.Types
                     ( FromJSON1, liftParseJSON, liftParseJSONList
                     , FromJSONKey, fromJSONKey, fromJSONKeyList
                     , FromJSONKeyFunction
                        ( FromJSONKeyCoerce
                        , FromJSONKeyText
                        , FromJSONKeyTextParser
                        , FromJSONKeyValue
                        )
                     , ToJSON1, liftToJSON, liftToJSONList
                     , ToJSONKey, toJSONKey, toJSONKeyList
                     , contramapToJSONKeyFunction
                     , Parser
                     )
#endif


-- base ----------------------------------------------------------------------
#if !MIN_VERSION_base(4, 7, 0)
import           Control.Applicative (empty)
#endif
import           Control.Monad (guard)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Traversable (traverse)
#endif
#if MIN_VERSION_aeson(1, 0, 0)
import           Unsafe.Coerce (unsafeCoerce)
#endif


-- types ---------------------------------------------------------------------
import           Data.Pi (Pi, fromPi, LoadPi (loadPi))
import           Type.Meta
                     ( Known, Val, val
                     , Proxy (Proxy), Sing (Sing), Some (Some)
#if defined(DataPolyKinds) && !defined(KindsAreTypes)
                     , KProxy
#endif
                     )


-- types-hashable ------------------------------------------------------------
import           Type.Meta.Hashable ()


------------------------------------------------------------------------------
instance (FromJSON r, LoadPi (TKind (KPoly1)) r a) =>
    FromJSON (Pi (TKind (KPoly1)) r a)
  where
    parseJSON a = parseJSON a >>= loadPi (Proxy :: Proxy a)


------------------------------------------------------------------------------
instance ToJSON r => ToJSON (Pi (TKind (KPoly1)) r a) where
    toJSON = toJSON . fromPi


------------------------------------------------------------------------------
instance (FromJSON r, Eq r, Known a, Val a ~ r) => FromJSON (Sing r a) where
    parseJSON a = do
        r <- parseJSON a
        guard $ r == val (Proxy :: Proxy a)
        return $ Sing Proxy


------------------------------------------------------------------------------
instance FromJSON r => FromJSON (Some r) where
    parseJSON = fmap return . parseJSON


------------------------------------------------------------------------------
instance ToJSON r => ToJSON (Sing r a) where
    toJSON (Sing a) = toJSON (val a)


------------------------------------------------------------------------------
instance ToJSON r => ToJSON (Some r) where
    toJSON (Some (Sing a)) = toJSON (val a)
#if !MIN_VERSION_base(4, 7, 0)


------------------------------------------------------------------------------
instance FromJSON (Proxy k) where
    parseJSON Null = return Proxy
    parseJSON _ = empty


------------------------------------------------------------------------------
instance ToJSON (Proxy k) where
    toJSON _ = Null
#endif
#if MIN_VERSION_aeson(1, 0, 0)


------------------------------------------------------------------------------
bind :: FromJSONKeyFunction a -> (a -> Parser b) -> FromJSONKeyFunction b
bind (FromJSONKeyCoerce _) k = FromJSONKeyTextParser $ k . unsafeCoerce
bind (FromJSONKeyText f) k = FromJSONKeyTextParser $ k . f
bind (FromJSONKeyTextParser f) k = FromJSONKeyTextParser $ \t -> f t >>= k
bind (FromJSONKeyValue f) k = FromJSONKeyValue $ \v -> f v >>= k


------------------------------------------------------------------------------
instance (FromJSONKey r, LoadPi (TKind (KPoly1)) r a) =>
    FromJSONKey (Pi (TKind (KPoly1)) r a)
  where
    fromJSONKey = bind fromJSONKey (loadPi (Proxy :: Proxy a))
    fromJSONKeyList = bind fromJSONKeyList
        (traverse (loadPi (Proxy :: Proxy a)))


------------------------------------------------------------------------------
instance ToJSONKey r => ToJSONKey (Pi (TKind (KPoly1)) r a) where
    toJSONKey = contramapToJSONKeyFunction fromPi toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (map fromPi) toJSONKeyList


------------------------------------------------------------------------------
instance (FromJSONKey r, Eq r, Known a, Val a ~ r) => FromJSONKey (Sing r a)
  where
    fromJSONKey = case fromJSONKey of
        FromJSONKeyCoerce _ -> FromJSONKeyTextParser $ \text -> do
            let r = unsafeCoerce text
            guard $ r == val (Proxy :: Proxy a)
            return $ Sing Proxy
        FromJSONKeyText f -> FromJSONKeyTextParser $ \text -> do
            let r = f text
            guard $ r == val (Proxy :: Proxy a)
            return $ Sing Proxy
        FromJSONKeyTextParser p -> FromJSONKeyTextParser $ \text -> do
            r <- p text
            guard $ r == val (Proxy :: Proxy a)
            return $ Sing Proxy
        FromJSONKeyValue p -> FromJSONKeyValue $ \value -> do
            r <- p value
            guard $ r == val (Proxy :: Proxy a)
            return $ Sing Proxy
    fromJSONKeyList = case fromJSONKeyList of
        FromJSONKeyCoerce _ -> FromJSONKeyTextParser $ \text -> do
            let r = unsafeCoerce text
            guard $ all (== val (Proxy :: Proxy a)) r
            return $ map (const (Sing Proxy)) r
        FromJSONKeyText f -> FromJSONKeyTextParser $ \text -> do
            let r = f text
            guard $ all (== val (Proxy :: Proxy a)) r
            return $ map (const (Sing Proxy)) r
        FromJSONKeyTextParser p -> FromJSONKeyTextParser $ \text -> do
            r <- p text
            guard $ all (== val (Proxy :: Proxy a)) r
            return $ map (const (Sing Proxy)) r
        FromJSONKeyValue p -> FromJSONKeyValue $ \value -> do
            r <- p value
            guard $ all (== val (Proxy :: Proxy a)) r
            return $ map (const (Sing Proxy)) r


------------------------------------------------------------------------------
instance FromJSONKey r => FromJSONKey (Some r) where
    fromJSONKey = fmap return fromJSONKey
    fromJSONKeyList = fmap (map return) fromJSONKeyList


------------------------------------------------------------------------------
instance ToJSONKey r => ToJSONKey (Sing r a) where
    toJSONKey = contramapToJSONKeyFunction singVal toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (map singVal) toJSONKeyList


------------------------------------------------------------------------------
instance ToJSONKey r => ToJSONKey (Some r) where
    toJSONKey = contramapToJSONKeyFunction someVal toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (map someVal) toJSONKeyList


------------------------------------------------------------------------------
instance FromJSON1 Some where
    liftParseJSON pj _ = fmap return . pj
    liftParseJSONList _ pjl = fmap (map return) . pjl


------------------------------------------------------------------------------
instance ToJSON1 Some where
    liftToJSON tj _ = tj . someVal
    liftToJSONList _ tjl = tjl . map someVal
#if !MIN_VERSION_base(4, 7, 0)


------------------------------------------------------------------------------
instance FromJSON1 Proxy where
    liftParseJSON _ _ Null = return Proxy
    liftParseJSON _ _ _ = empty


------------------------------------------------------------------------------
instance ToJSON1 Proxy where
    liftToJSON _ _ _ = Null
#endif


------------------------------------------------------------------------------
singVal :: Sing r a -> r
singVal (Sing a) = val a


------------------------------------------------------------------------------
someVal :: Some r -> r
someVal (Some (Sing a)) = val a
#endif
