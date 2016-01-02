{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

module Type.TH
    ( FromValue
    , type_
    , typeQ
    , proxy
    , proxyQ
    , synonym
    , synonymQ
    )
where

-- base ----------------------------------------------------------------------
import           Data.Bits (shiftR, testBit)
import qualified Data.Char as C (Char)
import           Data.String (IsString, fromString)
#if __GLASGOW_HASKELL__ >= 710
import qualified Numeric.Natural as N (Natural)
#endif
import           Prelude hiding (Char, String)
import qualified Prelude as S (String)


-- template-haskell ----------------------------------------------------------
import           Language.Haskell.TH
                     ( Dec (TySynD)
                     , Exp (SigE, ConE)
                     , Q
                     , Type
                         ( AppT
                         , ConT
#ifdef TypeLits
                         , LitT
#endif
                         )
#ifdef TypeLits
                     , TyLit (NumTyLit, StrTyLit)
#endif
                     , mkName
                     )


-- types ---------------------------------------------------------------------
import           Type.Bool (True, False)
import           Type.Char (Char)
import           Type.Either (Left, Right)
import           Type.List (Cons, Nil)
import           Type.Maybe (Just, Nothing)
import           Type.Meta (Proxy (Proxy))
import           Type.Natural (Natural)
import           Type.Num ((:+), (:*), (:-))
import           Type.Ordering (LT, EQ, GT)
import           Type.String (String)
import           Type.Tuple.Unit (Unit)
import           Type.Tuple.Pair (Pair)
import           Type.Tuple.Triplet (Triplet)
import           Type.Tuple.Quartet (Quartet)
import           Type.Tuple.Quintet (Quintet)
import           Type.Tuple.Sextet (Sextet)
import           Type.Tuple.Septet (Septet)


------------------------------------------------------------------------------
-- | Laws: @'Type.Meta.val' ('Proxy :: 'Proxy' $('proxyQ' a)) == a@
class FromValue a where
    type_ :: a -> Type


------------------------------------------------------------------------------
instance FromValue Bool where
    type_ False = ConT ''False
    type_ True = ConT ''True


------------------------------------------------------------------------------
instance FromValue Ordering where
    type_ LT = ConT ''LT
    type_ EQ = ConT ''EQ
    type_ GT = ConT ''GT


------------------------------------------------------------------------------
instance FromValue C.Char where
    type_ c = foldl AppT (ConT ''Char) (map (bit c) [0..31])
      where
        bit c n = ConT (if testBit (fromEnum c) n then ''True else ''False)


------------------------------------------------------------------------------
instance FromValue Integer where
    type_ i
        | i < 0 = error "Type level Integers not implemented yet, only Naturals"
        | otherwise = AppT (ConT ''Natural) (go i)
      where
        go 0 = ConT ''Nil
        go n = AppT (AppT (ConT ''Cons) (type_ (testBit n 0))) (go (shiftR n 1))


#if __GLASGOW_HASKELL__ >= 710
------------------------------------------------------------------------------
instance FromValue N.Natural where
    type_ = type_ . toInteger


#endif
------------------------------------------------------------------------------
instance FromValue a => FromValue [a] where
    type_ [] = ConT ''Nil
    type_ (a : as) = AppT (AppT (ConT ''Cons) (type_ a)) (type_ as)


------------------------------------------------------------------------------
instance FromValue a => FromValue (Maybe a) where
    type_ Nothing = ConT ''Nothing
    type_ (Just a) = AppT (ConT ''Just) (type_ a)


------------------------------------------------------------------------------
instance (FromValue a, FromValue b) => FromValue (Either a b) where
    type_ (Left a) = AppT (ConT ''Left) (type_ a)
    type_ (Right a) = AppT (ConT ''Right) (type_ a)


------------------------------------------------------------------------------
instance FromValue () where
    type_ () = ConT ''Unit


------------------------------------------------------------------------------
instance (FromValue a, FromValue b) => FromValue (a, b) where
    type_ (a, b) = AppT (AppT (ConT ''Pair) (type_ a)) (type_ b)


------------------------------------------------------------------------------
instance
    ( FromValue a
    , FromValue b
    , FromValue c
    )
  =>
    FromValue (a, b, c)
  where
    type_ (a, b, c) = foldl AppT (ConT ''Triplet)
        [ type_ a
        , type_ b
        , type_ c
        ]


------------------------------------------------------------------------------
instance
    ( FromValue a
    , FromValue b
    , FromValue c
    , FromValue d
    )
  =>
    FromValue (a, b, c, d)
  where
    type_ (a, b, c, d) = foldl AppT (ConT ''Quartet)
        [ type_ a
        , type_ b
        , type_ c
        , type_ d
        ]


------------------------------------------------------------------------------
instance
    ( FromValue a
    , FromValue b
    , FromValue c
    , FromValue d
    , FromValue e
    )
  =>
    FromValue (a, b, c, d, e)
  where
    type_ (a, b, c, d, e) = foldl AppT (ConT ''Quintet)
        [ type_ a
        , type_ b
        , type_ c
        , type_ d
        , type_ e
        ]


------------------------------------------------------------------------------
instance
    ( FromValue a
    , FromValue b
    , FromValue c
    , FromValue d
    , FromValue e
    , FromValue f
    )
  =>
    FromValue (a, b, c, d, e, f)
  where
    type_ (a, b, c, d, e, f) = foldl AppT (ConT ''Sextet)
        [ type_ a
        , type_ b
        , type_ c
        , type_ d
        , type_ e
        , type_ f
        ]


------------------------------------------------------------------------------
instance
    ( FromValue a
    , FromValue b
    , FromValue c
    , FromValue d
    , FromValue e
    , FromValue f
    , FromValue g
    )
  =>
    FromValue (a, b, c, d, e, f, g)
  where
    type_ (a, b, c, d, e, f, g) = foldl AppT (ConT ''Triplet)
        [ type_ a
        , type_ b
        , type_ c
        , type_ d
        , type_ e
        , type_ f
        , type_ g
        ]


------------------------------------------------------------------------------
typeQ :: FromValue a => a -> Q Type
typeQ = return . type_


------------------------------------------------------------------------------
proxy :: FromValue a => a -> Exp
proxy = makeProxy . type_


------------------------------------------------------------------------------
proxyQ :: FromValue a => a -> Q Exp
proxyQ = return . proxy


------------------------------------------------------------------------------
synonym :: FromValue a => S.String -> a -> [Dec]
synonym name value = [TySynD (mkName name) [] (type_ value)]


------------------------------------------------------------------------------
synonymQ :: FromValue a => S.String -> a -> Q [Dec]
synonymQ name value = return (synonym name value)


------------------------------------------------------------------------------
-- Cheeky orphan instances!
------------------------------------------------------------------------------


------------------------------------------------------------------------------
instance IsString (Q Type) where
#ifdef TypeLits
    fromString = return . LitT . StrTyLit
#else
    fromString = return . AppT (ConT ''String) . type_
#endif


------------------------------------------------------------------------------
instance IsString (Q Exp) where
    fromString = fmap makeProxy . fromString


#if __GLASGOW_HASKELL__ < 704
------------------------------------------------------------------------------
instance Show (Q Type) where
    showsPrec _ _ = showString "Q Type"


------------------------------------------------------------------------------
instance Eq (Q Type) where
    _ == _ = False


#endif
------------------------------------------------------------------------------
instance Num (Q Type) where
    a + b = do
        a' <- a
        b' <- b
        return $ AppT (AppT (ConT ''(:+)) a') b'
    a - b = do
        a' <- a
        b' <- b
        return $ AppT (AppT (ConT ''(:-)) a') b'
    a * b = do
        a' <- a
        b' <- b
        return $ AppT (AppT (ConT ''(:*)) a') b'
    abs a = a
    signum _ = fromInteger 1
#ifdef TypeLits
    fromInteger = return . LitT . NumTyLit
#else
    fromInteger = return . type_
#endif


#if __GLASGOW_HASKELL__ < 704
------------------------------------------------------------------------------
instance Show (Q Exp) where
    showsPrec _ _ = showString "Q Exp"


------------------------------------------------------------------------------
instance Eq (Q Exp) where
    _ == _ = False


#endif
------------------------------------------------------------------------------
instance Num (Q Exp) where
    a + b = fmap makeProxy $ unProxy a + unProxy b
    a - b = fmap makeProxy $ unProxy a - unProxy b
    a * b = fmap makeProxy $ unProxy a * unProxy b
    abs a = a
    signum _ = fromInteger 1
    fromInteger = fmap makeProxy . fromInteger


------------------------------------------------------------------------------
makeProxy :: Type -> Exp
makeProxy = SigE (ConE 'Proxy) . AppT (ConT ''Proxy)


------------------------------------------------------------------------------
unProxy :: Q Exp -> Q Type
unProxy exp = do
    e <- exp
    case e of
        SigE (ConE p) (AppT (ConT p') t)
            | p == 'Proxy && p' == ''Proxy -> return t
        _ -> fail $ "not a proxy: " ++ show e