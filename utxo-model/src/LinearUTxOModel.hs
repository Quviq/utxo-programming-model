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

-- minAda :: UTxO a -> Integer
-- minAda _ = 2_000_000

-- TODO:
-- * Figure out SmartContrat monad to force evaluation of "validators"
-- * Figure out module system stuff to force you to actually use the "validators"
-- * Signature PubKeyOwner can only come from the monad in some listigt sätt
--
{- The linear idea:

  We want to write "builder functions" that work on UTxOs to define contracts.
  However, we don't have a whole compiler back-end to use to turn these builder
  functions into actual validators. This is a problem because in order to emulate
  the chain we need to guarantee that our "pure functions transforming UTxO sets"
  (i.e. our view of transactions) are actually in comformance with the validators
  *implied by our builders*.

  Barring some magic the only real way to do this is to ensure that the only way
  to actually transform the UTxOs belonging to a given contract is to *use the
  actual builder functions* to build transactions that are value-preserving.

  Now, one could imagine that the way to do this is to only expose the builder
  functions as the destructors of "auction" or "escrow" UTxOs (using some
  type-index + module system magic and not allowing you to pattern-match on
  UTxOs). However, *this is not enough* because one might write a function like:

  cheat auctionUTxO = UTxO myWallet (valueOf auctionUTxO) ()

  that would be able to "steal" the money from the auction contract.
  You might think that you could avoid this by not exposing the `valueOf` function
  (which anyway isn't a workable solution!). But you still have the same problem!

  cheat auctionUTxO = UTxO myWallet (ada 10) ()

  This function would steal the value of the auction contract if and
  only if it happens to be 10 ada.

  The way around this is to use linear types to *guarantee* that any
  transaction (pure function from utxos to utxos) actually consumes
  the input transactions using the exposed builder functions!

  If we force transactions to be of a type like:

  cheat :: UTxO "auction" AuctionData -o UTxO SomeWallet ()

  we don't have this problem any more!

  Now, there are lots of details to work out but that's later Ulf and Max's problem.
  Now it's playtime.

  One more thing!

  To use a `UTxO SomeWallet ()` one would need something like a function of type
  `UTxO SomeWallet () -o ()` that would acutally consume the UTxO (you can't pattern
  match on UTxOs remember!). Now, no such function should exist in general but you could
  have a monadic action in the `SmartContract` monad that gives you this - something like:

  signedBy wallet1 $ \ destructor :: UTxO SomeWallet () -o () -> myTxCode destructor

  where `destructor` here would check that the input UTxO actually belongs to Wallet 1.

  ---

  One of many questions that remains to be answered is what the implications of this are
  when we go to our own stand-alone language? Clearly (?) we still need some form of linear
  type system - maybe it can be cleverererrere than Haskell's? Anyway, now it's *really*
  play time!

-}
