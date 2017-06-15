{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
                     )
#endif


-- base ----------------------------------------------------------------------
#if !MIN_VERSION_base(4, 7, 0)
import           Control.Applicative (empty)
#endif
import           Control.Monad (guard)
#if MIN_VERSION_aeson(1, 0, 0)
import           Unsafe.Coerce (unsafeCoerce)
#endif


-- types ---------------------------------------------------------------------
import           Data.Pi (Pi (Pi), fromPi, toPi)
import           Type.Maybe (Nothing, Just)
import           Type.Meta
                     ( Known, Val, val
                     , Proxy (Proxy), Sing (Sing), Some (Some)
                     )


-- types-hashable ------------------------------------------------------------
import           Type.Meta.Hashable ()


------------------------------------------------------------------------------
instance (FromJSON r, Eq r, Known a, Val a ~ r) => FromJSON (Pi (Just a) r)
  where
    parseJSON a = do
        r <- parseJSON a
        guard $ r == val (Proxy :: Proxy a)
        return $ Pi Proxy


------------------------------------------------------------------------------
instance FromJSON r => FromJSON (Pi Nothing r) where
    parseJSON = fmap return . parseJSON


------------------------------------------------------------------------------
instance ToJSON r => ToJSON (Pi a r) where
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
instance (FromJSONKey r, Eq r, Known a, Val a ~ r) =>
    FromJSONKey (Pi (Just a) r)
  where
    fromJSONKey = fmap singToPi fromJSONKey
      where
        singToPi :: Sing r a -> Pi (Just a) r
        singToPi (Sing p) = Pi p
    fromJSONKeyList = fmap (map singToPi) fromJSONKeyList
      where
        singToPi :: Sing r a -> Pi (Just a) r
        singToPi (Sing p) = Pi p


------------------------------------------------------------------------------
instance FromJSONKey r => FromJSONKey (Pi Nothing r) where
    fromJSONKey = fmap return fromJSONKey
    fromJSONKeyList = fmap (map return) fromJSONKeyList


------------------------------------------------------------------------------
instance ToJSONKey r => ToJSONKey (Pi a r) where
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
instance a ~ Nothing => FromJSON1 (Pi a) where
    liftParseJSON pj _ = fmap return . pj
    liftParseJSONList _ pjl = fmap (map return) . pjl


------------------------------------------------------------------------------
instance a ~ Nothing => ToJSON1 (Pi a) where
    liftToJSON tj _ = tj . fromPi
    liftToJSONList _ tjl = tjl . map fromPi


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
