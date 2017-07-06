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
#if !MIN_VERSION_base(4, 8, 0) && MIN_VERSION_aeson(1, 0, 0)
import           Data.Traversable (traverse)
#endif
#if MIN_VERSION_aeson(1, 0, 0)
import           Unsafe.Coerce (unsafeCoerce)
#endif


-- types ---------------------------------------------------------------------
import           Data.Pi (Pi, fromPi, LoadPi (loadPi))
import           Data.Sing
                     ( Some, fromSome, toSome
                     , Sing, fromSing, LoadSing (loadSing)
                     )
import           Type.Meta (Known, Val)
import           Type.Meta.Proxy
                     ( Proxy (Proxy)
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
    parseJSON a = parseJSON a >>= loadSing (Proxy :: Proxy a)


------------------------------------------------------------------------------
instance FromJSON r => FromJSON (Some (TKind (KPoly1)) r) where
    parseJSON = fmap toSome . parseJSON


------------------------------------------------------------------------------
instance ToJSON r => ToJSON (Sing r a) where
    toJSON = toJSON . fromSing


------------------------------------------------------------------------------
instance ToJSON r => ToJSON (Some (TKind (KPoly1)) r) where
    toJSON = toJSON . fromSome
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
instance (FromJSONKey r, LoadSing r a) => FromJSONKey (Sing r a) where
    fromJSONKey = bind fromJSONKey (loadSing (Proxy :: Proxy a))
    fromJSONKeyList = bind fromJSONKeyList
        (traverse (loadSing (Proxy :: Proxy a)))


------------------------------------------------------------------------------
instance FromJSONKey r => FromJSONKey (Some (TKind (KPoly1)) r) where
    fromJSONKey = fmap toSome fromJSONKey
    fromJSONKeyList = fmap (map toSome) fromJSONKeyList


------------------------------------------------------------------------------
instance ToJSONKey r => ToJSONKey (Sing r a) where
    toJSONKey = contramapToJSONKeyFunction fromSing toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (map fromSing) toJSONKeyList


------------------------------------------------------------------------------
instance ToJSONKey r => ToJSONKey (Some (TKind (KPoly1)) r) where
    toJSONKey = contramapToJSONKeyFunction fromSome toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (map fromSome) toJSONKeyList


------------------------------------------------------------------------------
instance FromJSON1 (Some (TKind (KPoly1))) where
    liftParseJSON pj _ = fmap toSome . pj
    liftParseJSONList _ pjl = fmap (map toSome) . pjl


------------------------------------------------------------------------------
instance ToJSON1 (Some (TKind (KPoly1))) where
    liftToJSON tj _ = tj . fromSome
    liftToJSONList _ tjl = tjl . map fromSome
#if !MIN_VERSION_base(4, 7, 0)


------------------------------------------------------------------------------
instance FromJSON1 Proxy where
    liftParseJSON _ _ Null = return Proxy
    liftParseJSON _ _ _ = empty


------------------------------------------------------------------------------
instance ToJSON1 Proxy where
    liftToJSON _ _ _ = Null
#endif
#endif
