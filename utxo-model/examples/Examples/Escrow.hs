-- This module defines an interface to the Escrow contract.
-- The actual contract is defined in the Escrow.Trusted module.
module Examples.Escrow where

import Prelude.Linear (($))
import Prelude hiding (($))

import Data.Functor.Linear qualified as LFunctor

import UTxO.Model

import Data.Typeable
import GHC.TypeLits
import Control.DeepSeq

import Examples.Escrow.Trusted

-- NOTE: the choice not to put together multiple UTxOs
-- here to make the value is just for simplicity.
--
-- Further note that because all this function does is pay
-- into the escrow it doesn't need to be defined in the "trusted"
-- module that defines the actual contract.
pay :: forall p d.
          (Typeable d, Show d, KnownSymbol p)
       => Signature PubKeyOwner
       -> PubKeyHash
       -> Value
       -> EscrowParams d
       -> UTxO PubKeyOwner ()
    %1 -> (UTxO (Escrow d p) PubKeyHash, Maybe (UTxO PubKeyOwner ()))
pay sign pkh value params utxo
  | checkParams @p params = (mkScriptUTxO value pkh, spendFromUTxO sign value utxo)
  | otherwise = failTx utxo "Incorrect EscrowParams"

payOffchain :: (Typeable d, Show d)
            => PubKeyHash
            -> Value
            -> EscrowParams d
            -> SmartContract ()
payOffchain pkh v params = onWallet pkh $ do
  SomeParams @p <- pure $ getParams params
  utxo <- findWalletUTxOWhere pkh (v `leq`)
  let payTx = withSignature pkh $ \ sign -> tx $ pay @p sign pkh v params
  () <$ submitTx payTx utxo

redeemOffchain :: forall d.
                  (Typeable d, Show d, NFData d)
               => EscrowParams d
               -> SmartContract ()
redeemOffchain params = do
  SomeParams @p <- pure $ getParams params
  now <- getTime
  let addr = mkScriptAddress @(Escrow d p)
      redeemTx = withTime now (escrowDeadline params - 1) $ \ time ->
                 tx $ redeemInternal @p params time
  utxos <- index @(Escrow d p) @PubKeyHash addr
  () <$ submitTx redeemTx utxos


refundOffchain :: forall d.
                  (Typeable d, Show d, NFData d)
               => PubKeyHash
               -> EscrowParams d
               -> SmartContract ()
refundOffchain pkh params = onWallet pkh $ do
  SomeParams @p <- pure $ getParams params
  now <- getTime
  let addr = mkScriptAddress @(Escrow d p)
      refundTx = withTime (escrowDeadline params) (10 + max now (escrowDeadline params)) $ \ time ->
                 withSignature pkh $ \ sign ->
                 tx $ LFunctor.fmap @[] (refundInternal @p params time sign)
  utxos <- index @(Escrow d p) @PubKeyHash addr
  () <$ submitTx refundTx utxos
