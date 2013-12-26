{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
#if __GLASGOW_HASKELL__ == 706
{-# LANGUAGE PolyKinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Type.Symbol
    ( proxy
    , type_
    , synonym
    )
where

-- base ----------------------------------------------------------------------
import           Data.String (IsString (fromString))

-- template-haskell ----------------------------------------------------------
import           Language.Haskell.TH
                     ( Q
                     , Exp (SigE, ConE)
                     , Type (ConT, AppT)
                     , Dec (TySynD)
                     , mkName
                     )

#if __GLASGOW_HASKELL__ < 706
-- base ----------------------------------------------------------------------
import           Data.Bits (testBit)
import           Prelude hiding (Char)

-- symbols -------------------------------------------------------------------
import           Type.Symbol.Internal (O, I, Char, Cons, Nil)
#else
-- template-haskell ----------------------------------------------------------
import           Language.Haskell.TH (Type (LitT), TyLit (StrTyLit))
#endif

#if __GLASGOW_HASKELL__ >= 707
-- base ----------------------------------------------------------------------
import           Data.Proxy (Proxy (Proxy))
#else

------------------------------------------------------------------------------
data Proxy s = Proxy deriving (Eq, Ord, Read, Show)
#endif


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
type_ = foldr (AppT . AppT (ConT ''Cons) . char) (ConT ''Nil)
  where
    char c = foldl AppT (ConT ''Char) (map (bit c) [0..31])
    bit c n = ConT (if testBit (fromEnum c) n then ''I else ''O)
#endif


------------------------------------------------------------------------------
synonym :: String -> String -> Q [Dec]
synonym name symbol = return [TySynD (mkName name) [] (type_ symbol)]
