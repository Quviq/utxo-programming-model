module UTxO.Value where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Group
import Data.Maybe

data ValueKey = Ada
              | Token String
              deriving (Ord, Eq, Show)

newtype Value = Value { unValue :: Map ValueKey Integer }
              deriving (Ord, Show)

instance Eq Value where
  v == v' = all (==0) $ unValue (v ~~ v')

adaOf :: Value -> Integer
adaOf = fromMaybe 0 . Map.lookup Ada . unValue

instance Semigroup Value where
  Value m <> Value m' = Value (Map.unionWith (+) m m')

instance Monoid Value where
  mempty = Value mempty

instance Group Value where
  invert = Value . fmap negate . unValue
