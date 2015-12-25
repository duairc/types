{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Type.Symbol.TH
    ( KnownSymbol
    , symbolVal
    , proxy
    , type_
    , synonym
    )
where

-- base ----------------------------------------------------------------------
#if __GLASGOW_HASKELL__ < 706
import           Data.Bits (testBit)
#endif
import           Data.String (IsString (fromString))
import           Data.Proxy (Proxy (Proxy))
#if __GLASGOW_HASKELL__ >= 708
import           GHC.TypeLits (KnownSymbol, symbolVal)
#endif


#if __GLASGOW_HASKELL__ < 708
-- typelits-compat -----------------------------------------------------------
#if __GLASGOW_HASKELL__ < 706
import           Type.Symbol.Compat (O, I, C, (:::), Nil)
#endif
import           GHC.TypeLits.Compat (KnownSymbol, symbolVal)


#endif
-- template-haskell ----------------------------------------------------------
import           Language.Haskell.TH
                     ( Dec (TySynD)
                     , Exp (SigE, ConE)
                     , Q
                     , Type
                         ( AppT
                         , ConT
#if __GLASGOW_HASKELL__ >= 706
                         , LitT
#endif
                         )
#if __GLASGOW_HASKELL__ >= 706
                     , TyLit (StrTyLit)
#endif
                     , mkName
                     )


------------------------------------------------------------------------------
instance IsString (Q Exp) where
    fromString = return . proxy


------------------------------------------------------------------------------
instance IsString (Q Type) where
    fromString = return . type_


------------------------------------------------------------------------------
proxy :: String -> Exp
proxy = SigE (ConE 'Proxy) . AppT (ConT ''Proxy) . type_


------------------------------------------------------------------------------
type_ :: String -> Type
#if __GLASGOW_HASKELL__ >= 706
type_ = LitT . StrTyLit
#else
type_ = foldr (AppT . AppT (ConT ''(:::)) . char) (ConT ''Nil)
  where
    char c = foldl AppT (ConT ''C) (map (bit c) [0..31])
    bit c n = ConT (if testBit (fromEnum c) n then ''I else ''O)
#endif


------------------------------------------------------------------------------
synonym :: String -> String -> Q [Dec]
synonym name symbol = return [TySynD (mkName name) [] (type_ symbol)]
