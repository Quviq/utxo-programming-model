module UTxOModel where

import Value

data UTxO a = NoUTxO
            | UTxO Value a
            deriving (Ord, Eq, Show)

newtype UTxORef a = UTxORef { unUTxORef :: Integer }

data SmartContract a where
  Done       :: a -> SmartContract a
  Atomically :: (UTxO a -> UTxO b)
             -> UTxORef a
             -> (UTxORef b -> SmartContract c)
             -> SmartContract c

deriving instance Functor SmartContract
