{-# LANGUAGE UndecidableInstances #-}
module UTxO.Model
  ( -- * Dealing with values
    module UTxO.Value
  -- * Writing Validators
  -- ** UTxOs
  , UTxO
  , PubKeyHash(..)
  , Script
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
  , useUTxOs
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
  , UTxORef
  , Transaction
  , IsTx(..)
  , tx
  , withTime
  , withSignature
  , submitTx
  , lookupUTxO
  , index
  , findWalletUTxOWhere
  , onWallet
  , awaitTime
  , getTime
  -- * Linearity helpers
  , let'
  -- * Script Parameters
  , ParamWitness
  , SomeParams(..)
  , checkParams
  , getParams
  ) where

import Prelude.Linear (($))
import Prelude hiding (($))

import Control.Monad

import Data.Group
import Data.Unrestricted.Linear
import Data.Typeable
import GHC.TypeLits

import UTxO.Value
import UTxO.Trusted

let' :: a %1 -> (a %1 -> b) %1 -> b
let' a f = f a

addressOf :: UTxO owner datum %1 -> (Ur Address, UTxO owner datum)
addressOf utxo = let' (matchUTxO utxo) $ \ (a, Ur _, Ur _, utxo) -> (a, utxo)

useUTxO :: UTxO owner datum
     %1 -> Signature owner
        -> Ur (Address, Value, datum)
useUTxO utxo sign =
  let' (matchUTxO utxo)      $ \ (Ur a, Ur v, Ur d, utxo) ->
  let' (spendUTxO utxo sign) $ \ () ->
  Ur (a, v, d)

useUTxOs :: [UTxO owner datum]
      %1 -> Signature owner
         -> Ur [(Address, Value, datum)]
useUTxOs [] _                = Ur []
useUTxOs (utxo : utxos) sign =
  lift2 (:) (useUTxO utxo sign) (useUTxOs utxos sign)

spendFromUTxO :: IsOwner owner
              => Signature owner
              -> Value
              -> UTxO owner datum
           %1 -> Maybe (UTxO owner datum)
spendFromUTxO sign v utxo =
  let' (useUTxO utxo sign) $ \ (Ur (a, v', d)) ->
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

findWalletUTxOWhere :: PubKeyHash
                    -> (Value -> Bool)
                    -> SmartContract (UTxORef PubKeyOwner ())
findWalletUTxOWhere pkh p = do
  let isValidUTxO ref = do
        Just (_, v, _) <- lookupUTxO ref
        return (p v)
  utxo : _ <- filterM isValidUTxO =<< index (Wallet pkh)
  return utxo

-- NOTE: this is a bit hacky and we want a nicer solution in a stand-alone
-- language.
type ParamWitness = Symbol
type IsParams p = Show p

checkParams :: forall (w :: ParamWitness) p.
               (IsParams p, KnownSymbol w)
            => p -> Bool
checkParams p = show p == symbolVal @w Proxy

data SomeParams where
  SomeParams :: forall (s :: ParamWitness). KnownSymbol s => SomeParams

getParams :: IsParams p => p -> SomeParams
getParams p | SomeSymbol @s _ <- someSymbolVal (show p) = SomeParams @s
