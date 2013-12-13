{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
#if __GLASGOW_HASKELL__ == 706
{-# LANGUAGE PolyKinds #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Type.Symbol () where

-- base ----------------------------------------------------------------------
import           Data.String (IsString (fromString))
#if __GLASGOW_HASKELL__ < 706
import           Data.Bits (testBit)
import           Prelude hiding (Char)
#endif
#if __GLASGOW_HASKELL__ >= 707
import           Data.Proxy (Proxy (Proxy))
#endif

-- template-haskell ----------------------------------------------------------
import           Language.Haskell.TH (Q, Exp (SigE, ConE), Type (ConT, AppT))
#if __GLASGOW_HASKELL__ >= 706
import           Language.Haskell.TH (Type (LitT), TyLit (StrTyLit))
#endif


#if __GLASGOW_HASKELL__ < 707
------------------------------------------------------------------------------
data Proxy s = Proxy deriving (Show)
#endif


------------------------------------------------------------------------------
instance IsString (Q Exp) where
    fromString = fmap (SigE (ConE 'Proxy) . AppT (ConT ''Proxy)) . fromString


------------------------------------------------------------------------------
instance IsString (Q Type) where
#if __GLASGOW_HASKELL__ >= 706
    fromString = return . LitT . StrTyLit
#else
    fromString = return . go
      where
        go [] = ConT ''Nil
        go (c:cs) = AppT (AppT (ConT ''Cons) (char c)) (go cs)
        char c = foldl AppT (ConT ''Char) (map (bit c) [0..31])
        bit c n = if testBit (fromEnum c) n then ConT ''One else ConT ''Zero


------------------------------------------------------------------------------
data Zero


------------------------------------------------------------------------------
data One


------------------------------------------------------------------------------
data Char
    bit_0  bit_1  bit_2  bit_3  bit_4  bit_5  bit_6  bit_7
    bit_8  bit_9  bit_10 bit_11 bit_12 bit_13 bit_14 bit_15
    bit_16 bit_17 bit_18 bit_19 bit_20 bit_21 bit_22 bit_23
    bit_24 bit_25 bit_26 bit_27 bit_28 bit_29 bit_30 bit_31


------------------------------------------------------------------------------
data Cons char symbol


------------------------------------------------------------------------------
data Nil
#endif
