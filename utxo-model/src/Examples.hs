module Examples where

import UTxOModel

fib :: UTxO (Int, Int) -> UTxO (Int, Int)
fib NoUTxO                = UTxO "fib" mempty (1, 1)
fib (UTxO "fib" v (n, m)) = UTxO "fib" v (m, m + n)
fib utxo                  = error $ "fib " ++ show utxo

driveFib :: Int -> SmartContract Int
driveFib n = do
  utxo <- findUTxO "fib"
  go n utxo
  where
    go 0 utxo = snd <$> observe utxo
    go n utxo = do
      utxo' <- tx fib utxo
      go (n - 1) utxo'
