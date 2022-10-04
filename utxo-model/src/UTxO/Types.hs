{-# LANGUAGE LinearTypes #-}
module UTxO.Types where

import Data.Typeable

data Dict c where
  Dict :: c => Dict c

-- Just a little bit of dependent types bro. It won't hurt you bro. Just try it bro.
data TList2 :: (* -> * -> *) -> [*] -> * where
  Nil  :: TList2 f '[]
  Cons :: f a b %1 -> TList2 f ts %1 -> TList2 f ((a, b) : ts)

tList2Append :: TList2 f xs %1 -> TList2 f ys %1 -> TList2 f (Append xs ys)
tList2Append Nil xs         = xs
tList2Append (Cons f xs) ys = Cons f (tList2Append xs ys)

type family Append (xs :: [*]) (ys :: [*]) :: [*] where
  Append '[] xs      = xs
  Append (x : xs) ys = x : (Append xs ys)

newtype MaybeF2 f a b = MaybeF2 { unMaybeF2 :: Maybe (f a b) }

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
appendTypeListDict = error "TODO"
