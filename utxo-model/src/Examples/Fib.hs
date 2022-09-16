module Examples.Fib where

import UTxOModel

fib :: UTxO (Int, Int) -> UTxO (Int, Int)
fib NoUTxO                = UTxO "fib" mempty (1, 1)
fib (UTxO "fib" v (n, m)) = UTxO "fib" v (m, m + n)
fib utxo                  = error $ "fib " ++ show utxo

unfib :: UTxO (Int, Int) -> UTxO ()
unfib NoUTxO           = NoUTxO
unfib (UTxO "fib" v _) = UTxO (Wallet 1) v ()
unfib utxo             = error $ "unfib " ++ show utxo

driveFib :: Int -> SmartContract Int
driveFib n = do
  utxos <- findUTxOs "fib"
  case utxos of
    []       -> new >>= go n
    (utxo:_) -> go n utxo
  where
    go 0 utxo = snd <$> observe utxo
    go n utxo = do
      utxo' <- tx fib utxo
      go (n - 1) utxo'
