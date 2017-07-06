{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

module Symbols
    ( DataFin
    , DataPi
    , DataSing
    , DataVect
    , Fin
    , Pi
    , Sing
    , Some
    , Types
    , Vect
    )
where

#ifdef UseTypeLits
------------------------------------------------------------------------------
type DataFin = "Data.Fin"
type DataPi = "Data.Pi"
type DataSing = "Data.Sing"
type DataVect = "Data.Vect"
type Fin = "Fin"
type Pi = "Pi"
type Sing = "Sing"
type Some = "Some"
type Types = "types"
type Vect = "Vect"
#else
-- types ---------------------------------------------------------------------
import           Type.Bool (True, False)
import           Type.Char (Char)
import           Type.List (Nil, Cons)
import           Type.String (String)


------------------------------------------------------------------------------
type I = True
type O = False
type a :+ as = Cons a as
infixr 5 :+


------------------------------------------------------------------------------
type DataFin = String (-- $("Data.Fin")
    Char O O I O O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I O O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I O O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
type DataPi = String (-- $("Data.Pi")
    Char O O I O O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I O O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O O O I O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
type DataSing = String (-- $("Data.Sing")
    Char O O I O O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I O O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I O O I O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
type DataVect = String (-- $("Data.Vect")
    Char O O I O O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I O O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I O I O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
type Fin = String (-- $("Fin")
    Char O I I O O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
type Pi = String (-- $("Pi")
    Char O O O O I O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
type Sing = String (-- $("Sing")
    Char I I O O I O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
type Some = String (-- $("Some")
    Char I I O O I O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
type Types = String (-- $("types")
    Char O O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O I I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O O O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I O O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
type Vect = String (-- $("Vect")
    Char O I I O I O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
#endif
