{-# LANGUAGE NoImplicitPrelude #-}
module Examples.PlutusUseCases.EscrowTrusted where

import Prelude.Linear (($), Ur(..))
import Prelude hiding (($))

import GHC.Generics
import Control.DeepSeq

import UTxO.Model

data Escrow = Escrow
  deriving stock (Show, Generic)
  deriving anyclass NFData

instance IsOwner Escrow where
  fresh _ = Just Escrow

own :: Signature Escrow
own Escrow = ()

-- NOTE: the whole datum business is a bit
-- orthogonal here so we ignore it for now
data EscrowTarget =
  EscrowTarget
    { targetAddress :: Address
    , targetValue   :: Value
    }

data EscrowParams =
  EscrowParams
    { escrowDeadline :: Time
    , escrowTargets  :: [EscrowTarget]
    }

targetTotal :: EscrowParams -> Value
targetTotal = foldMap targetValue . escrowTargets

redeemInternal :: EscrowParams
               -> TrueTime
               -> [UTxO Escrow PubKeyHash]
            %1 -> [UTxO AnyOwner ()]
redeemInternal params validity utxos
  | upperBound validity < escrowDeadline params  =
      let' (useUTxOs utxos own) $ \ (Ur avds) ->
      if targetTotal params `leq` mconcat [ v | (_, v, _) <- avds ]
      then [ mkUTxO (targetAddress t) (targetValue t) () | t <- escrowTargets params ]
      else failTx () "Not enough value"
  | otherwise = failTx utxos "Deadline has passed."

refundInternal :: EscrowParams
               -> TrueTime
               -> Signature PubKeyOwner
               -> UTxO Escrow PubKeyHash
           %1  -> UTxO PubKeyOwner ()
refundInternal params validity signature utxo
  | escrowDeadline params < lowerBound validity =
      let' (useUTxO utxo own) $ \ (Ur (_, v, contributor)) ->
      let' (checkSignature signature contributor) $ \ () ->
      mkUTxO (Wallet contributor) v ()
  | otherwise = failTx utxo "Deadline has not passed yet"
