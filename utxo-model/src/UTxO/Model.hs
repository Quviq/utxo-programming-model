{-# LANGUAGE UndecidableInstances #-}
module UTxO.Model
  ( -- * Dealing with values
    module UTxO.Value
  -- * Writing Validators
  -- ** UTxOs
  , UTxO
  , PubKeyHash(..)
  , Address(..)
  -- ** Ownership
  , IsOwner(..)
  , PubKeyOwner
  , AnyOwner
  , Signature
  -- ** Consuming UTxOs
  , spendUTxO
  , matchUTxO
  , useUTxO
  , spendFromUTxO
  , castUTxO
  , addressOf
  -- ** Failing transactions
  , checkSignature
  , failTx
  -- ** Creating UTxOs
  , mkUTxO
  , mkScriptAddress
  , mkScriptUTxO
  , mkPubKeyUTxO
  -- ** Dealing with time
  , TrueTime
  , Time
  , lowerBound
  , upperBound
  -- * Writing smart contracts
  , SmartContract
  , UTxORef(..)
  , Transaction(..)
  , IsTx(..)
  , tx
  , withTime
  , withSignature
  , submitTx
  , lookupUTxO
  , index
  , findWalletUTxOWhere
  -- * Linearity helpers
  , let'
  ) where

import Prelude.Linear (($))
import Prelude hiding (($))

import Control.Monad

import Data.Group
import Data.Unrestricted.Linear
import Data.Typeable

import UTxO.Value
import UTxO.Trusted hiding (withTime, withSignature)
import UTxO.Trusted qualified

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

spendFromUTxO :: IsOwner owner
              => Signature owner
              -> Value
              -> UTxO owner datum
           %1 -> Maybe (UTxO owner datum)
spendFromUTxO sign v utxo =
  let' (useUTxO utxo sign) $ \ (Ur a, Ur v', Ur d) ->
  if | v' == v   -> Nothing
     | otherwise -> Just $ mkUTxO a (v' <> invert v) d

mkScriptAddress :: forall (owner :: *). Typeable owner => Address
mkScriptAddress = Script $ typeRep @[] @owner []

mkScriptUTxO :: forall owner datum. IsOwner owner => Value -> datum -> UTxO owner datum
mkScriptUTxO = mkUTxO (mkScriptAddress @owner)

mkPubKeyUTxO :: PubKeyHash -> Value -> UTxO PubKeyOwner ()
mkPubKeyUTxO hash value = mkUTxO (Wallet hash) value ()

checkSignature :: Signature PubKeyOwner -> PubKeyHash -> ()
checkSignature sign hash = case fresh $ Wallet hash of
  Nothing    -> failTx () "The impossible happened"
  Just owner -> sign owner

withSignature :: PubKeyHash -> (Signature PubKeyOwner -> Transaction t) -> Transaction t
withSignature pkh tx = UTxO.Trusted.withSignature pkh tx

withTime :: Time -> Time -> (TrueTime -> Transaction t) -> Transaction t
withTime t0 t1 tx = UTxO.Trusted.withTime t0 t1 tx

findWalletUTxOWhere :: PubKeyHash
                    -> (Value -> Bool)
                    -> SmartContract (UTxORef PubKeyOwner ())
findWalletUTxOWhere pkh p = do
  let isValidUTxO ref = do
        Just (_, v, _) <- lookupUTxO ref
        return (p v)
  utxo : _ <- filterM isValidUTxO =<< index (Wallet pkh)
  return utxo

