{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
#endif

#ifdef SafeHaskell
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 800
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
#endif

#include "kinds.h"

module Type.Char
    (
#ifdef DataPolyKinds
      KChar
    , Char
#else
      Char
#endif
    )
where

-- base ----------------------------------------------------------------------
import           Data.Bits ((.|.), shiftL)
import qualified Data.Char as C (Char)
#if __GLASGOW_HASKELL__ >= 708 && defined(DataPolyKinds)
import           Data.Type.Equality (type (==))
import           Data.Typeable (Typeable)
#endif
import           Prelude hiding (Char)


--- types --------------------------------------------------------------------
import           Type.Bool ((:&&))
import           Type.Eq ((:==))
import           Type.Meta (Known, val, Proxy (Proxy))
import           Type.Ord (Compare)
import           Type.Semigroup ((:<>))


------------------------------------------------------------------------------
#ifdef DataPolyKinds
type Char = 'Char


------------------------------------------------------------------------------
data KChar = Char
    !KBool !KBool !KBool !KBool !KBool !KBool !KBool !KBool
    !KBool !KBool !KBool !KBool !KBool !KBool !KBool !KBool
    !KBool !KBool !KBool !KBool !KBool !KBool !KBool !KBool
    !KBool !KBool !KBool !KBool !KBool !KBool !KBool !KBool
#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable Char
#endif
#else
data Char
    (b00 :: KBool) (b01 :: KBool) (b02 :: KBool) (b03 :: KBool)
    (b04 :: KBool) (b05 :: KBool) (b06 :: KBool) (b07 :: KBool)
    (b08 :: KBool) (b09 :: KBool) (b10 :: KBool) (b11 :: KBool)
    (b12 :: KBool) (b13 :: KBool) (b14 :: KBool) (b15 :: KBool)
    (b16 :: KBool) (b17 :: KBool) (b18 :: KBool) (b19 :: KBool)
    (b20 :: KBool) (b21 :: KBool) (b22 :: KBool) (b23 :: KBool)
    (b24 :: KBool) (b25 :: KBool) (b26 :: KBool) (b27 :: KBool)
    (b28 :: KBool) (b29 :: KBool) (b30 :: KBool) (b31 :: KBool)
#endif


------------------------------------------------------------------------------
instance
    ( Known Bool b00
    , Known Bool b01
    , Known Bool b02
    , Known Bool b03
    , Known Bool b04
    , Known Bool b05
    , Known Bool b06
    , Known Bool b07
    , Known Bool b08
    , Known Bool b09
    , Known Bool b10
    , Known Bool b11
    , Known Bool b12
    , Known Bool b13
    , Known Bool b14
    , Known Bool b15
    , Known Bool b16
    , Known Bool b17
    , Known Bool b18
    , Known Bool b19
    , Known Bool b20
    , Known Bool b21
    , Known Bool b22
    , Known Bool b23
    , Known Bool b24
    , Known Bool b25
    , Known Bool b26
    , Known Bool b27
    , Known Bool b28
    , Known Bool b29
    , Known Bool b30
    , Known Bool b31
    )
  =>
    Known C.Char (Char
        b00 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15
        b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
  where
    val _ = do
        let bits =
             [ val (Proxy :: Proxy b00)
             , val (Proxy :: Proxy b01)
             , val (Proxy :: Proxy b02)
             , val (Proxy :: Proxy b03)
             , val (Proxy :: Proxy b04)
             , val (Proxy :: Proxy b05)
             , val (Proxy :: Proxy b06)
             , val (Proxy :: Proxy b07)
             , val (Proxy :: Proxy b08)
             , val (Proxy :: Proxy b09)
             , val (Proxy :: Proxy b10)
             , val (Proxy :: Proxy b11)
             , val (Proxy :: Proxy b12)
             , val (Proxy :: Proxy b13)
             , val (Proxy :: Proxy b14)
             , val (Proxy :: Proxy b15)
             , val (Proxy :: Proxy b16)
             , val (Proxy :: Proxy b17)
             , val (Proxy :: Proxy b18)
             , val (Proxy :: Proxy b19)
             , val (Proxy :: Proxy b20)
             , val (Proxy :: Proxy b21)
             , val (Proxy :: Proxy b22)
             , val (Proxy :: Proxy b23)
             , val (Proxy :: Proxy b24)
             , val (Proxy :: Proxy b25)
             , val (Proxy :: Proxy b26)
             , val (Proxy :: Proxy b27)
             , val (Proxy :: Proxy b28)
             , val (Proxy :: Proxy b29)
             , val (Proxy :: Proxy b30)
             , val (Proxy :: Proxy b31)
             ]
        toEnum $ foldr (\b c -> shiftL c 1 .|. if b then 1 else 0) 0 bits
    {-# INLINE val #-}


#if __GLASGOW_HASKELL__ >= 708 && defined(DataPolyKinds)
------------------------------------------------------------------------------
type instance (a :: KChar) == (b :: KChar) = a :== b


#endif
------------------------------------------------------------------------------
type instance (:==)
    (Char
        a00 a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14 a15
        a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31)
    (Char
        b00 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15
        b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31) =
            a00 :== b00 :&& a01 :== b01 :&& a02 :== b02 :&& a03 :== b03 :&&
            a04 :== b04 :&& a05 :== b05 :&& a06 :== b06 :&& a07 :== b07 :&&
            a08 :== b08 :&& a09 :== b09 :&& a10 :== b10 :&& a11 :== b11 :&&
            a12 :== b12 :&& a13 :== b13 :&& a14 :== b14 :&& a15 :== b15 :&&
            a16 :== b16 :&& a17 :== b17 :&& a18 :== b18 :&& a19 :== b19 :&&
            a20 :== b20 :&& a21 :== b21 :&& a22 :== b22 :&& a23 :== b23 :&&
            a24 :== b24 :&& a25 :== b25 :&& a26 :== b26 :&& a27 :== b27 :&&
            a28 :== b28 :&& a29 :== b29 :&& a30 :== b30 :&& a31 :== b31


------------------------------------------------------------------------------
type instance Compare
    (Char
        a00 a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14 a15
        a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31)
    (Char
        b00 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15
        b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31) =
            Compare a31 b31 :<> Compare a30 b30 :<> Compare a29 b29 :<>
            Compare a28 b28 :<> Compare a27 b27 :<> Compare a26 b26 :<>
            Compare a25 b25 :<> Compare a24 b24 :<> Compare a23 b23 :<>
            Compare a22 b22 :<> Compare a21 b21 :<> Compare a20 b20 :<>
            Compare a19 b19 :<> Compare a18 b18 :<> Compare a17 b17 :<>
            Compare a16 b16 :<> Compare a15 b15 :<> Compare a14 b14 :<>
            Compare a13 b13 :<> Compare a12 b12 :<> Compare a11 b11 :<>
            Compare a10 b10 :<> Compare a09 b09 :<> Compare a08 b08 :<>
            Compare a07 b07 :<> Compare a06 b06 :<> Compare a05 b05 :<>
            Compare a04 b04 :<> Compare a03 b03 :<> Compare a02 b02 :<>
            Compare a01 b01 :<> Compare a00 b00