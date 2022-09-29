{-# LANGUAGE LinearTypes #-}
module LinearUTxOModel
  ( -- * Writing Validators
    PubKeyHash(..)
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
  -- * Dealing with time
  , TrueTime
  , Time
  , lowerBound
  , upperBound
  -- * Writing smart contracts
  , SmartContract
  , TxRep
  , UTxOs
  , MaybeUTxOs
  , UTxORef(..)
  , UTxORefs
  , MaybeUTxORefs
  , transform
  , withSignature
  , withTime
  , submitTx
  ) where

import Control.Monad

import Data.Typeable

import Value
import Types

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

type Time = Integer -- Slots

-- NOTE: It's important that this talk about both an upper and a lower
-- bound on time if we want to turn this into transactions. If this
-- were just `newtype TrueTime = TrueTime { getTime :: Time }` it's
-- not clear how to turn that into a function that works in a given
-- time interval.
data TrueTime = TrueTime { lowerBound :: Time, upperBound :: Time }
  deriving (Ord, Eq, Show)

-- TODO:
-- With this model there might be a problem with multiple transactions happening in
-- the same call to `submitTx`. However, if we type index UTxOs by a "phase" - giving us an
-- "input utxo" and an "output utxo" type we would be able to enforce only one
-- stage of transformation per transaction:
-- tx :: (UTxOs n %1 -> UTxOs (Succ n)) -> TxRepType

type UTxOs = TList2 UTxO
type MaybeUTxOs = TList2 (MaybeF2 UTxO)

newtype UTxORef owner datum = UTxORef { getRef :: Int }

type UTxORefs = TList2 UTxORef
type MaybeUTxORefs = TList2 (MaybeF2 UTxORef)

data TxRep inputs outputs where
  Transform     :: (UTxOs inputs %1 -> MaybeUTxOs outputs) -> TxRep inputs outputs
  WithSignature :: PubKeyHash -> (Signature PubKeyOwner -> TxRep inputs outputs) -> TxRep inputs outputs
  WithTime      :: Time -> Time -> (TrueTime -> TxRep inputs outputs) -> TxRep inputs outputs

data SmartContract a where
  Done   :: a -> SmartContract a
  Submit :: TxRep inputs outputs -> UTxORefs inputs -> (MaybeUTxORefs outputs -> SmartContract a) -> SmartContract a

transform :: (UTxOs inputs %1 -> MaybeUTxOs outputs) -> TxRep inputs outputs
transform = Transform

withSignature :: PubKeyHash -> (Signature PubKeyOwner -> TxRep inputs outputs) -> TxRep inputs outputs
withSignature = WithSignature

withTime :: Time -> Time -> (TrueTime -> TxRep inputs outputs) -> TxRep inputs outputs
withTime = WithTime

submitTx :: TxRep inputs outputs -> UTxORefs inputs -> SmartContract (MaybeUTxORefs outputs)
submitTx tx is = Submit tx is Done

instance Functor SmartContract where
  fmap = liftM

instance Applicative SmartContract where
  pure = Done
  (<*>) = ap

instance Monad SmartContract where
  Done a         >>= k = k a
  Submit tx is c >>= k = Submit tx is (c >=> k)
