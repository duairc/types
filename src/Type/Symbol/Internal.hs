{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif

module Type.Symbol.Internal
    ( O
    , I
    , Char
    , Cons
    , Nil
    )
where


------------------------------------------------------------------------------
data O


------------------------------------------------------------------------------
data I


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
