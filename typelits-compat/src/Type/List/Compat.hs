{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif

module Type.List.Compat
    ( (:::)
    , Nil
    )
where


------------------------------------------------------------------------------
data x ::: xs
infixr 5 :::


------------------------------------------------------------------------------
data Nil