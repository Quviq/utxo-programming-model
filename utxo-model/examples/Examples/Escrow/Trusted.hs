{-# LANGUAGE NoImplicitPrelude #-}
module Examples.Escrow.Trusted
  ( Escrow
  , EscrowTarget(..)
  , EscrowParams(..)
  , targetAddress
  , targetValue
  , targetUTxO
  , targetTotal
  , redeemInternal
  , refundInternal
  ) where

import Prelude.Linear (($), Ur(..))
import Prelude hiding (($))

import GHC.Generics
import GHC.TypeLits
import Control.DeepSeq
import Data.Typeable

import UTxO.Model

-- NOTE: this ParamWitness business
-- is a bit of a hack to get around the
-- fact that GHC doesn't support modules
-- parameterized on values. It may be that
-- a stand-alone language can support this
-- in a much nicer way.
data Escrow (d :: *) (p :: ParamWitness) = Escrow
  deriving stock (Show, Generic)
  deriving anyclass NFData

instance (Typeable d, KnownSymbol p) => IsOwner (Escrow d p) where
  fresh _ = Just Escrow

own :: Signature (Escrow d p)
own Escrow = ()

data EscrowTarget d =
      PaymentPubKeyTarget PubKeyHash Value
    | ScriptTarget Script d Value
    deriving Show

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
    } deriving Show

targetTotal :: EscrowParams d -> Value
targetTotal = foldMap targetValue . escrowTargets

redeemInternal :: forall p d.
                  (Show d, KnownSymbol p)
               => EscrowParams d
               -> TrueTime
               -> [UTxO (Escrow d p) PubKeyHash]
            %1 -> [Either (UTxO AnyOwner d) (UTxO PubKeyOwner ())]
redeemInternal params validity utxos
  -- NOTE: Because this is a prototype
  -- this check is here because we represent
  -- scripts on both the type and term level at the
  -- same time. In this prototype a script is identified by
  -- the type `Escrow d p` where `p` "morally" represents
  -- a unique `EscrowParams d` but we need to check that
  -- this is true. In Plutus this check is built-in because
  -- the params are compiled in to the script (so different
  -- params => different script hash). We envision that a
  -- stand-alone language should be able to do something
  -- similar to avoid this.
  --
  -- One might imagine that we do something similar to
  -- PubKeyOwner instead and put the parameters in script
  -- addresses. That's something to think about as it would
  -- remove the need for these ugly checks. Again, prototype.
  | not $ checkParams @p params = failTx utxos "Incorrect EscrowParams provided"
  | upperBound validity < escrowDeadline params  =
      let' (useUTxOs utxos own) $ \ (Ur avds) ->
      if targetTotal params `leq` mconcat [ v | (_, v, _) <- avds ]
      then map targetUTxO (escrowTargets params)
      else failTx () "Not enough value"
  | otherwise = failTx utxos "Deadline has passed."

refundInternal :: forall p d.
                  (Show d, KnownSymbol p)
               => EscrowParams d
               -> TrueTime
               -> Signature PubKeyOwner
               -> UTxO (Escrow d p) PubKeyHash
           %1  -> UTxO PubKeyOwner ()
refundInternal params validity signature utxo
  | not $ checkParams @p params = failTx utxo "Incorrect EscrowParams provided"
  | escrowDeadline params < lowerBound validity =
      let' (useUTxO utxo own) $ \ (Ur (_, v, contributor)) ->
      let' (checkSignature signature contributor) $ \ () ->
      mkUTxO (Wallet contributor) v ()
  | otherwise = failTx utxo "Deadline has not passed yet"
