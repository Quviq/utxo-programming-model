module Examples where

import UTxOModel

fib :: UTxO (Int, Int) -> UTxO (Int, Int)
fib NoUTxO                = UTxO "fib" mempty (1, 1)
fib (UTxO "fib" v (n, m)) = UTxO "fib" v (m, m + n)
fib utxo                  = error $ "fib " ++ show utxo

unfib :: UTxO (Int, Int) -> UTxO ()
unfib (UTxO "fib" v _) = UTxO (Wallet 1) v ()
unfib _                = NoUTxO

-- TODO:
-- In this model - how do you know what function
-- can be applied to what UTxO and how do we model
-- that failure in our modelling framework??
--
-- Maybe we need something like:
--  (fibTx :: Tx (UTxO (Int, Int) -> UTxO (Int, Int)), unfibTx :: Tx (UTxO (Int, Int) -> UTxO ())) <- bind (fib, unfib) "fib"
-- In the monad? This would be what creates
-- the script "fib" - no other functions are allowed to interact with "fib" UTxOs?
-- How do we compose functions that do several things at the same time? How do we deal
-- with the whole composition thing here?
--
-- It would be nice to write compostions that work on several scripts as just nicely
-- composed haskell functions. But if we need a notion of a `Tx` rather than a simple
-- function to act as the "only ok way to use 'fib'" then don't we miss some
-- compositionality? How do we use compositional functions that act across two "related"
-- scripts? They somehow need to be "bound" twice to two different script names?
--
-- We also still need to get the notion of "who can do what" in here somehow. How would that
-- work?

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
