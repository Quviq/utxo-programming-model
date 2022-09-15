module Examples where

import Vealue
import UTxOModel

fib :: UTxO (Int, Int) -> UTxO (Int, Int)
fib NoUTxO = UTxO mempty (1, 1)
fib (UTxO v (n, m)) = UTxO v (m, m + n)
