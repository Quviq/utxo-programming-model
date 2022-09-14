module Value where

import Data.Map (Map)
import Data.Map qualified as Map

data ValueKey = Ada
              | Token String
              deriving (Ord, Eq, Show)

newtype Value = Value { unValue :: Map ValueKey Integer }
              deriving (Ord, Eq, Show)
