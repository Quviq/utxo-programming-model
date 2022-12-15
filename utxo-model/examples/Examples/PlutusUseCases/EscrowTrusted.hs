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

data EscrowTarget d =
      PaymentPubKeyTarget PubKeyHash Value
    | ScriptTarget Script d Value

targetAddress :: EscrowTarget d -> Address
targetAddress (PaymentPubKeyTarget h _) = Wallet h
targetAddress (ScriptTarget s _ _)      = Script s

targetValue :: EscrowTarget d -> Value
targetValue (PaymentPubKeyTarget _ v) = v
targetValue (ScriptTarget _ _ v) = v

targetUTxO :: EscrowTarget d -> Either (UTxO AnyOwner d) (UTxO PubKeyOwner ())
targetUTxO t@PaymentPubKeyTarget{} = Right $ mkUTxO (targetAddress t) (targetValue t) ()
targetUTxO t@(ScriptTarget _ d _) = Left $ mkUTxO (targetAddress t) (targetValue t) d

data EscrowParams d =
  EscrowParams
    { escrowDeadline :: Time
    , escrowTargets  :: [EscrowTarget d]
    }

targetTotal :: EscrowParams d -> Value
targetTotal = foldMap targetValue . escrowTargets

redeemInternal :: EscrowParams d
               -> TrueTime
               -> [UTxO Escrow PubKeyHash]
            %1 -> [Either (UTxO AnyOwner d) (UTxO PubKeyOwner ())]
redeemInternal params validity utxos
  | upperBound validity < escrowDeadline params  =
      let' (useUTxOs utxos own) $ \ (Ur avds) ->
      if targetTotal params `leq` mconcat [ v | (_, v, _) <- avds ]
      then map targetUTxO (escrowTargets params)
      else failTx () "Not enough value"
  | otherwise = failTx utxos "Deadline has passed."

refundInternal :: EscrowParams d
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
