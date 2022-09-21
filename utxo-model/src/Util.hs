{-# LANGUAGE LinearTypes #-}
module Util where

import Prelude.Linear (($))
import Prelude hiding (($))

import Data.Group
import Data.Unrestricted.Linear
import Data.Typeable

import Value

import LinearUTxOModel

let' :: a %1 -> (a %1 -> b) %1 -> b
let' a f = f a

addressOf :: UTxO owner datum %1 -> (Ur Address, UTxO owner datum)
addressOf utxo = let' (matchUTxO utxo) $ \ (a, Ur _, Ur _, utxo) -> (a, utxo)

useUTxO :: UTxO owner datum
     %1 -> Signature owner
        -> (Ur Address, Ur Value, Ur datum)
useUTxO utxo sign =
  let' (matchUTxO utxo)      $ \ (a, v, d, utxo) ->
  let' (spendUTxO utxo sign) $ \ () ->
  (a, v, d)

spendFrom :: IsOwner owner
          => Signature owner
          -> Value
          -> UTxO owner datum
       %1 -> Maybe (UTxO owner datum)
spendFrom sign v utxo =
  let' (useUTxO utxo sign) $ \ (Ur a, Ur v', Ur d) ->
  if | v' == v   -> Nothing
     | otherwise -> Just $ mkUTxO a (v' <> invert v) d

mkScriptAddress :: Typeable owner => owner -> Address
mkScriptAddress = Script . typeOf

mkScriptUTxO :: forall owner datum. IsOwner owner => Value -> datum -> UTxO owner datum
mkScriptUTxO = mkUTxO (mkScriptAddress $ fresh @owner)

mkPubKeyUTxO :: PubKeyHash -> Value -> UTxO PubKeyOwner ()
mkPubKeyUTxO hash value = mkUTxO (Wallet hash) value ()

checkSignature :: Signature PubKeyOwner -> PubKeyHash -> ()
checkSignature sign hash = case fresh $ Wallet hash of
  Nothing    -> failTx () "The impossible happened"
  Just owner -> sign owner
