{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module LinearUTxOModel
  ( PubKeyHash(..)
  , Address(..)
  , UTxO
  , IsOwner(..)
  , PubKeyOwner
  , AnyOwner
  , Signature
  , TrueTime
  , Time
  , lowerBound
  , upperBound
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

type Time = Integer -- Slots

-- NOTE: It's important that this talk about both an upper and a lower
-- bound on time if we want to turn this into transactions. If this
-- were just `newtype TrueTime = TrueTime { getTime :: Time }` it's
-- not clear how to turn that into a function that works in a given
-- time interval.
data TrueTime = TrueTime { lowerBound :: Time, upperBound :: Time }
  deriving (Ord, Eq, Show)

-- TODO:
-- This is the overall idea of how to build transactions and *true* script contexts:
--
-- submit :: TxRepType -> SmartContract () (or whatever)
--
-- withSignature :: PubKeyHash -> (Signature PubKeyOwner -> TxRepType) -> TxRepTyp
-- withTime :: Time -> Time -> (TrueTime -> TxRepType) -> TxRepType
-- transform :: (UTxOs %1 -> UTxOs) -> TxRepType
--
-- The monad can insert the correct time and check that we are currently running on the
-- wallet that can sign for a given pub key hash etc.
--
-- The validator-builders meanwhile can use the `lowerBound :: TrueTime -> Time`
-- and `upperBound :: TrueTime -> Time` functions to check properties on time.

-- TODO:
-- With this model there might be a problem with multiple transactions happening in
-- the same transaction. However, if we type index UTxOs by a "phase" - giving us an
-- "input utxo" and an "output utxo" type we would be able to enforce only one
-- stage of transformation per transaction:
-- tx :: (UTxOs n %1 -> UTxOs (Succ n)) -> TxRepType

-- Just a little bit of dependent types bro. It won't hurt you bro. Just try it bro.
data TList2 :: (* -> * -> *) -> [*] -> * where
  Nil :: TList2 f '[]
  Cons :: f a b -> TList2 f ts -> TList2 f ((a, b) : ts)

tList2Append :: TList2 f xs -> TList2 f ys -> TList2 f (Append xs ys)
tList2Append Nil xs         = xs
tList2Append (Cons f xs) ys = Cons f (tList2Append xs ys)

type family Append (xs :: [*]) (ys :: [*]) :: [*] where
  Append '[] xs      = xs
  Append (x : xs) ys = x : (Append xs ys)

newtype MaybeF2 f a b = MaybeF2 { unMaybeF2 :: Maybe (f a b) }

type UTxOs = TList2 UTxO
type MaybeUTxOs = TList2 (MaybeF2 UTxO)

class Result a where
  type Res a :: [*]
  toResult :: a -> TList2 (MaybeF2 UTxO) (Res a)

instance Result (UTxO owner datum) where
  type Res (UTxO owner datum) = (owner, datum) : '[]
  toResult utxo = Cons (MaybeF2 $ Just utxo) Nil

instance Result (Maybe (UTxO owner datum)) where
  type Res (Maybe (UTxO owner datum)) = (owner, datum) : '[]
  toResult mutxo = Cons (MaybeF2 mutxo) Nil

instance (Result a, Result b) => Result (a, b) where
  type Res (a, b) = Append (Res a) (Res b)
  toResult (a, b) = tList2Append (toResult a) (toResult b)

instance (Result a, Result b, Result c) => Result (a, b, c) where
  type Res (a, b, c) = Append (Res a) (Append (Res b) (Res c))
  toResult (a, b, c) = tList2Append (toResult a) (tList2Append (toResult b) (toResult c))

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
