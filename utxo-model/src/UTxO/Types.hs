{-# LANGUAGE LinearTypes #-}
module UTxO.Types where

import Data.Typeable

data Dict c where
  Dict :: c => Dict c

data TList :: [*] -> * where
  Nil  :: TList '[]
  Cons :: a %1 -> TList as %1 -> TList (a ': as)

tListAppend :: TList xs %1 -> TList ys %1 -> TList (Append xs ys)
tListAppend Nil         ys = ys
tListAppend (Cons x xs) ys = Cons x (tListAppend xs ys)

type family Append (xs :: [*]) (ys :: [*]) :: [*] where
  Append '[] xs      = xs
  Append (x : xs) ys = x : (Append xs ys)

data SingletonList (ts :: [*]) where
  SingletonNil  :: SingletonList '[]
  SingletonCons :: forall t ts. IsTypeList ts => SingletonList (t : ts)

class IsTypeList (ts :: [*]) where
  materialize :: SingletonList ts

instance IsTypeList '[] where
  materialize = SingletonNil

instance IsTypeList xs => IsTypeList (x : xs) where
  materialize = SingletonCons

appendRightUnitProof :: forall a. IsTypeList a => Append a '[] :~: a
appendRightUnitProof = case materialize @a of
  SingletonNil -> Refl
  SingletonCons @_ @as
    | Refl <- appendRightUnitProof @as -> Refl

appendAssocProof :: forall a b c. IsTypeList a => Append a (Append b c) :~: Append (Append a b) c
appendAssocProof = case materialize @a of
  SingletonNil -> Refl
  SingletonCons @_ @as
    | Refl <- appendAssocProof @as @b @c -> Refl

appendTypeListDict :: forall as bs. (IsTypeList as, IsTypeList bs)
                   => Dict (IsTypeList (Append as bs))
appendTypeListDict = case materialize @as of
  SingletonNil -> Dict
  SingletonCons @_ @as'
    | Dict <- appendTypeListDict @as' @bs -> Dict
