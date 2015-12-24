{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif

module Type.Symbol.Internal
    ( O, I
    , C
    , (:::), Nil
    , KnownSymbol
    , symbolVal
    )
where

-- base ----------------------------------------------------------------------
import           Data.Bits ((.|.), shiftL)
import           Data.Char (chr)


------------------------------------------------------------------------------
data O


------------------------------------------------------------------------------
data I


------------------------------------------------------------------------------
data C
    bit_0  bit_1  bit_2  bit_3  bit_4  bit_5  bit_6  bit_7
    bit_8  bit_9  bit_10 bit_11 bit_12 bit_13 bit_14 bit_15
    bit_16 bit_17 bit_18 bit_19 bit_20 bit_21 bit_22 bit_23
    bit_24 bit_25 bit_26 bit_27 bit_28 bit_29 bit_30 bit_31


------------------------------------------------------------------------------
data char ::: symbol
infixr 5 :::


------------------------------------------------------------------------------
data Nil


------------------------------------------------------------------------------
data Bit = O | I


------------------------------------------------------------------------------
class KnownBit b where
    bitVal :: proxy b -> Bit


------------------------------------------------------------------------------
instance KnownBit O where
    bitVal _ = O
    {-# INLINE bitVal #-}


------------------------------------------------------------------------------
instance KnownBit I where
    bitVal _ = I
    {-# INLINE bitVal #-}


------------------------------------------------------------------------------
class KnownChar c where
    charVal :: proxy c -> Char


------------------------------------------------------------------------------
instance
    ( KnownBit bit_0
    , KnownBit bit_1
    , KnownBit bit_2
    , KnownBit bit_3
    , KnownBit bit_4
    , KnownBit bit_5
    , KnownBit bit_6
    , KnownBit bit_7
    , KnownBit bit_8
    , KnownBit bit_9
    , KnownBit bit_10
    , KnownBit bit_11
    , KnownBit bit_12
    , KnownBit bit_13
    , KnownBit bit_14
    , KnownBit bit_15
    , KnownBit bit_16
    , KnownBit bit_17
    , KnownBit bit_18
    , KnownBit bit_19
    , KnownBit bit_20
    , KnownBit bit_21
    , KnownBit bit_22
    , KnownBit bit_23
    , KnownBit bit_24
    , KnownBit bit_25
    , KnownBit bit_26
    , KnownBit bit_27
    , KnownBit bit_28
    , KnownBit bit_29
    , KnownBit bit_30
    , KnownBit bit_31
    )
  =>
    KnownChar (C bit_0 bit_1 bit_2 bit_3 bit_4 bit_5 bit_6 bit_7 bit_8 bit_9
        bit_10 bit_11 bit_12 bit_13 bit_14 bit_15 bit_16 bit_17 bit_18 bit_19
        bit_20 bit_21 bit_22 bit_23 bit_24 bit_25 bit_26 bit_27 bit_28 bit_29
        bit_30 bit_31)
  where
    charVal _ = do
        let bits =
             [ bitVal (Proxy :: Proxy bit_0)
             , bitVal (Proxy :: Proxy bit_1)
             , bitVal (Proxy :: Proxy bit_2)
             , bitVal (Proxy :: Proxy bit_3)
             , bitVal (Proxy :: Proxy bit_4)
             , bitVal (Proxy :: Proxy bit_5)
             , bitVal (Proxy :: Proxy bit_6)
             , bitVal (Proxy :: Proxy bit_7)
             , bitVal (Proxy :: Proxy bit_8)
             , bitVal (Proxy :: Proxy bit_9)
             , bitVal (Proxy :: Proxy bit_10)
             , bitVal (Proxy :: Proxy bit_11)
             , bitVal (Proxy :: Proxy bit_12)
             , bitVal (Proxy :: Proxy bit_13)
             , bitVal (Proxy :: Proxy bit_14)
             , bitVal (Proxy :: Proxy bit_15)
             , bitVal (Proxy :: Proxy bit_16)
             , bitVal (Proxy :: Proxy bit_17)
             , bitVal (Proxy :: Proxy bit_18)
             , bitVal (Proxy :: Proxy bit_19)
             , bitVal (Proxy :: Proxy bit_20)
             , bitVal (Proxy :: Proxy bit_21)
             , bitVal (Proxy :: Proxy bit_22)
             , bitVal (Proxy :: Proxy bit_23)
             , bitVal (Proxy :: Proxy bit_24)
             , bitVal (Proxy :: Proxy bit_25)
             , bitVal (Proxy :: Proxy bit_26)
             , bitVal (Proxy :: Proxy bit_27)
             , bitVal (Proxy :: Proxy bit_28)
             , bitVal (Proxy :: Proxy bit_29)
             , bitVal (Proxy :: Proxy bit_30)
             , bitVal (Proxy :: Proxy bit_31)
             ]
        chr $ foldr (\b c -> shiftL c 1 .|. case b of {O -> 0; I -> 1}) 0 bits
    {-# INLINE charVal #-}


------------------------------------------------------------------------------
class KnownSymbol n where
    symbolVal :: proxy n -> String


------------------------------------------------------------------------------
instance KnownSymbol Nil where
    symbolVal _ = ""
    {-# INLINE symbolVal #-}


------------------------------------------------------------------------------
instance (KnownChar c, KnownSymbol cs) => KnownSymbol (c ::: cs) where
    symbolVal _ = charVal (Proxy :: Proxy c) : symbolVal (Proxy :: Proxy cs)
    {-# INLINE symbolVal #-}


------------------------------------------------------------------------------
data Proxy a = Proxy
