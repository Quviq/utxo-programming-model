{-# LANGUAGE LinearTypes #-}
module UTxO.Types where

import Data.Typeable

-- Just a little bit of dependent types bro. It won't hurt you bro. Just try it bro.
data TList2 :: (* -> * -> *) -> [*] -> * where
  Nil :: TList2 f '[]
  Cons :: f a b %1 -> TList2 f ts %1 -> TList2 f ((a, b) : ts)

tList2Append :: TList2 f xs %1 -> TList2 f ys %1 -> TList2 f (Append xs ys)
tList2Append Nil xs         = xs
tList2Append (Cons f xs) ys = Cons f (tList2Append xs ys)

type family Append (xs :: [*]) (ys :: [*]) :: [*] where
  Append '[] xs      = xs
  Append (x : xs) ys = x : (Append xs ys)

newtype MaybeF2 f a b = MaybeF2 { unMaybeF2 :: Maybe (f a b) }

appendRightUnitProof :: forall a. Append a '[] :~: a
appendRightUnitProof = error "TODO"

appendAssocProof :: forall a b c. Append a (Append b c) :~: Append (Append a b) c
appendAssocProof = error "TODO"
