{-# LANGUAGE LinearTypes #-}
module UTxO.Types where

import Data.Typeable

data Dict c where
  Dict :: c => Dict c

-- data List2 a b = Nil2 | Cons2 a b (List2 a b)

-- Just a little bit of dependent types bro. It won't hurt you bro. Just try it bro.
-- data TList2 :: (* -> * -> *) -> List2 * * -> * where
--   Nil  :: TList  'Nil2
--   Cons :: f a b %1 -> TList  ts %1 -> TList  ('Cons2 a b ts)

data TList :: [*] -> * where
  Nil  :: TList '[]
  Cons :: a %1 -> TList as %1 -> TList (a ': as)

-- tList2Append :: TList  xs %1 -> TList  ys %1 -> TList  (Append xs ys)
-- tList2Append Nil xs         = xs
-- tList2Append (Cons f xs) ys = Cons f (tList2Append xs ys)

tListAppend :: TList xs %1 -> TList ys %1 -> TList (Append xs ys)
tListAppend Nil         ys = ys
tListAppend (Cons x xs) ys = Cons x (tListAppend xs ys)

type family Append (xs :: [*]) (ys :: [*]) :: [*] where
  Append '[] xs      = xs
  Append (x : xs) ys = x : (Append xs ys)

-- newtype MaybeF  a b = MaybeF2 { unMaybeF2 :: Maybe (f a b) }

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
