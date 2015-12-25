{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Type.Nat.TH
    ( KnownNat
    , natVal
    , proxy
    , type_
    , synonym
    )
where

-- base ----------------------------------------------------------------------
import           Data.Proxy (Proxy (Proxy))
#if __GLASGOW_HASKELL__ >= 708
import           GHC.TypeLits (KnownNat, natVal)
#endif


#if __GLASGOW_HASKELL__ < 708
-- typelits-compat -----------------------------------------------------------
import           Type.Nat.Compat (Z, S, KnownNat, natVal)


#endif
-- template-haskell ----------------------------------------------------------
import           Language.Haskell.TH
                     ( Dec (TySynD)
                     , Exp (SigE, ConE)
                     , Q
                     , Type
                         ( AppT
                         , ConT
#if __GLASGOW_HASKELL__ >= 708
                         , LitT
#endif
                         )
#if __GLASGOW_HASKELL__ >= 708
                     , TyLit (NumTyLit)
#endif
                     , mkName
                     )


#if __GLASGOW_HASKELL__ < 704
------------------------------------------------------------------------------
instance Eq (Q Exp) where
    a == b = undefined


------------------------------------------------------------------------------
instance Show (Q Exp) where
    show _ = "Q Exp"


------------------------------------------------------------------------------
instance Eq (Q Type) where
    a == b = undefined


------------------------------------------------------------------------------
instance Show (Q Type) where
    show _ = "Q Type"


#endif
------------------------------------------------------------------------------
instance Num (Q Exp) where
    fromInteger = return . proxy
    (+) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined


------------------------------------------------------------------------------
instance Num (Q Type) where
    fromInteger = return . type_
    (+) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined


------------------------------------------------------------------------------
proxy :: Integer -> Exp
proxy = SigE (ConE 'Proxy) . AppT (ConT ''Proxy) . type_


------------------------------------------------------------------------------
type_ :: Integer -> Type
#if __GLASGOW_HASKELL__ >= 708
type_ = LitT . NumTyLit
#else
type_ = ($ ConT ''Z) . foldr (.) id . flip replicate (AppT (ConT ''S))
    . fromInteger
#endif


------------------------------------------------------------------------------
synonym :: String -> Integer -> Q [Dec]
synonym name symbol = return [TySynD (mkName name) [] (type_ symbol)]
