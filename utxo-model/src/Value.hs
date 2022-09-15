module Value where

import Data.Map (Map)
import Data.Map qualified as Map

data ValueKey = Ada
              | Token String
              deriving (Ord, Eq, Show)

newtype Value = Value { unValue :: Map ValueKey Integer }
              deriving (Ord, Eq, Show)

instance Semigroup Value where
  Value m <> Value m' = Value (Map.unionWith (+) m m')

instance Monoid Value where
  mempty = Value mempty
