{-# LANGUAGE LinearTypes #-}
module LinearUTxOModel
  ( PubKeyHash(..)
  , Address(..)
  , UTxO
  , IsOwner(..)
  , PubKeyOwner
  , AnyOwner
  , Signature
  , mkUTxO
  , spendUTxO
  , matchUTxO
  , failTx
  , castUTxO
  ) where

import Data.String
import Data.Group
import Data.Typeable

import Value

import Unsafe.Linear qualified as Unsafe
import Data.Unrestricted.Linear

newtype PubKeyHash = PubKeyHash { unPubKeyHash :: Int }
  deriving (Ord, Eq, Show)

data Address where
  Script :: TypeRep -> Address
  Wallet :: PubKeyHash -> Address
  deriving (Ord, Eq, Show)

class Typeable owner => IsOwner owner where
  fresh :: Address -> Maybe owner

data AnyOwner where
  AnyOwner :: Address -> AnyOwner

instance IsOwner AnyOwner where
  fresh = Just . AnyOwner

data PubKeyOwner = PubKeyOwner PubKeyHash

instance IsOwner PubKeyOwner where
  fresh (Wallet hash) = Just $ PubKeyOwner hash
  fresh _             = Nothing

isAddressOf :: IsOwner owner => Address -> owner -> Bool
isAddressOf _ owner | typeOf owner == typeOf AnyOwner = True
isAddressOf (Script rep) owner = typeOf owner == rep
isAddressOf (Wallet pubKeyHash) owner = case cast owner of
  Just (PubKeyOwner hash) -> hash == pubKeyHash
  _ -> False

data UTxO owner datum where
  UTxO :: owner %1 -> Address -> Value -> datum -> UTxO owner datum
  deriving (Ord, Eq, Show)

mkUTxO :: IsOwner owner => Address -> Value -> datum -> UTxO owner datum
mkUTxO addr =
  case fresh addr of
    Just owner | addr `isAddressOf` owner -> UTxO owner addr
    _ -> failTx () "Owner and address don't match."

type Signature owner = owner %1 -> ()

spendUTxO :: UTxO owner datum %1 -> Signature owner -> ()
spendUTxO (UTxO o _ _ _) sign = sign o

matchUTxO :: UTxO a d %1 -> (Ur Address, Ur Value, Ur d, UTxO a d)
matchUTxO (UTxO s a v d) = (Ur a, Ur v, Ur d, UTxO s a v d)

failTx :: a %1 -> String -> b
failTx = Unsafe.toLinear (const error)

castUTxO :: IsOwner owner
         => UTxO AnyOwner datum
      %1 -> UTxO owner datum
-- NOTE: important to use mkUTxO here to get the check that addr matches owner
castUTxO (UTxO (AnyOwner addr) addr' value datum)
  | addr == addr' = mkUTxO addr' value datum
  | otherwise     = failTx () "castUTxO failed."

-- TODO:
-- This is the overall idea of how to build transactions and *true* script contexts:
--
-- submit :: TxRepType -> SmartContract () (or whatever)
--
-- withSignature :: PubKeyHash -> (Signature PubKeyOwner -> TxRepType) -> TxRepTyp
-- withTime :: (TrueTime -> TxRepType) -> TxRepType
-- transform :: (UTxOs %1 -> UTxOs) -> TxRepType
--
-- The monad can insert the correct time and check that we are currently running on the
-- wallet that can sign for a given pub key hash etc.
