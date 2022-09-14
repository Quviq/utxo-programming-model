module UTxOModel where

import Value

data UTxO a = NoUTxO
            | UTxO Value a
            deriving (Ord, Eq, Show)
