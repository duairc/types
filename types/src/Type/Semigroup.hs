{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#include "kinds.h"

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Safe #-}
#endif

module Type.Semigroup
    ( (:<>)
    )
where

------------------------------------------------------------------------------
type family (a :: KPoly1) :<> (b :: KPoly1) :: KPoly1
infixr 6 :<>
#ifdef DataPolyKinds


------------------------------------------------------------------------------
type instance '[] :<> '[] = '[]
type instance (a ': as) :<> '[] = (a ': as)
type instance '[] :<> (b ': bs) = (b ': bs)
type instance (a ': as) :<> (b ': bs) = a ': (as :<> (b ': bs))


------------------------------------------------------------------------------
type instance 'Nothing :<> 'Nothing = 'Nothing
type instance 'Nothing :<> 'Just b = 'Just b
type instance 'Just a :<> 'Nothing = 'Just a
type instance 'Just a :<> 'Just b = 'Just (a :<> b)


------------------------------------------------------------------------------
type instance 'Left _a :<> 'Left b = 'Left b
type instance 'Left _a :<> 'Right b = 'Right b
type instance 'Right a :<> 'Left _b = 'Right a
type instance 'Right a :<> 'Right _b = 'Right a


------------------------------------------------------------------------------
type instance '() :<> '() = '()


------------------------------------------------------------------------------
type instance '(a, b) :<> '(a', b') = '(a :<> a', b :<> b')


------------------------------------------------------------------------------
type instance '(a, b, c) :<> '(a', b', c') = '(a :<> a', b :<> b', c :<> c')


------------------------------------------------------------------------------
type instance '(a, b, c, d) :<> '(a', b', c', d') =
    '(a :<> a', b :<> b', c :<> c', d :<> d')


------------------------------------------------------------------------------
type instance '(a, b, c, d, e) :<> '(a', b', c', d', e') =
    '(a :<> a', b :<> b', c :<> c', d :<> d', e :<> e')


------------------------------------------------------------------------------
type instance '(a, b, c, d, e, f) :<> '(a', b', c', d', e', f') =
    '(a :<> a', b :<> b', c :<> c', d :<> d', e :<> e', f :<> f')


------------------------------------------------------------------------------
type instance '(a, b, c, d, e, f, g) :<> '(a', b', c', d', e', f', g') =
    '(a :<> a', b :<> b', c :<> c', d :<> d', e :<> e', f :<> f', g :<> g')
#endif
