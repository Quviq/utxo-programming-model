module Types where

-- Just a little bit of dependent types bro. It won't hurt you bro. Just try it bro.
data TList2 :: (* -> * -> *) -> [*] -> * where
  Nil :: TList2 f '[]
  Cons :: f a b -> TList2 f ts -> TList2 f ((a, b) : ts)

tList2Append :: TList2 f xs -> TList2 f ys -> TList2 f (Append xs ys)
tList2Append Nil xs         = xs
tList2Append (Cons f xs) ys = Cons f (tList2Append xs ys)

type family Append (xs :: [*]) (ys :: [*]) :: [*] where
  Append '[] xs      = xs
  Append (x : xs) ys = x : (Append xs ys)

newtype MaybeF2 f a b = MaybeF2 { unMaybeF2 :: Maybe (f a b) }
