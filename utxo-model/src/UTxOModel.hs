-- TODO: we don't know if this makes *any fucking sense*
module UTxOModel where

import Control.Monad

import Data.Typeable
import Data.String

import Value

data Some (f :: * -> *) where
  Some :: Typeable a => f a -> Some f

data Address = Script String
             | Wallet Int
             deriving (Ord, Eq, Show)

instance IsString Address where
  fromString = Script

-- TODO: no notion of what scripts owns this UTxO - do we need that?
-- What about UTxOs given to wallets?
-- Is there some way to get around the "address" notion??
data UTxO a = NoUTxO
            | UTxO Address Value a
            deriving (Ord, Eq, Show)

addressOf :: UTxO a -> Address
addressOf NoUTxO       = error "addressOf"
addressOf (UTxO a _ _) = a

minAda :: UTxO a -> Integer
minAda _ = 2_000_000

data UTxORef a = NoUTxORef
               | UTxORef Integer
               deriving (Ord, Eq, Show)

data SmartContract a where
  Done       :: a -> SmartContract a
  Atomically :: ([Some UTxO] -> [Some UTxO])
             -> [Some UTxORef]
             -> ([Some UTxORef] -> SmartContract c)
             -> SmartContract c
  Observe    :: UTxORef a
             -> (a -> SmartContract b)
             -> SmartContract b
  FindUTxOs  :: Typeable a
             => Address
             -> ([UTxORef a] -> SmartContract b)
             -> SmartContract b

deriving instance Functor SmartContract

instance Applicative SmartContract where
  pure = Done
  (<*>) = ap

instance Monad SmartContract where
  Done a            >>= k = k a
  Atomically f as c >>= k = Atomically f as (c >=> k)
  Observe ref c     >>= k = Observe ref     (c >=> k)
  FindUTxOs addr c  >>= k = FindUTxOs addr  (c >=> k)

class TxType a where
  type Contract a
  txify  :: a -> [Some UTxO] -> [Some UTxO]
  atomic :: ([Some UTxO] -> [Some UTxO]) -> [Some UTxORef] -> Contract a

instance Typeable a => TxType (UTxO a) where
  type Contract (UTxO a) = SmartContract (UTxORef a)

  txify a [] = [Some a]
  txify _ _  = error "txify: expecting no arguments to mkTx @(UTxO a)"

  atomic txfun args = Atomically txfun args cont
    where cont [Some (cast -> Just a)] = Done a
          cont _                       = error "Unexpected number of results"

instance (Typeable a, Typeable b) => TxType (UTxO a, UTxO b) where
  type Contract (UTxO a, UTxO b) = SmartContract (UTxORef a, UTxORef b)

  txify (a, b) [] = [Some a, Some b]
  txify _ _  = error "txify: expecting no arguments to mkTx @(UTxO a, UTxO b)"

  atomic txfun args = Atomically txfun args cont
    where cont [Some (cast -> Just a), Some (cast -> Just b)] = Done (a, b)
          cont _                                              = error "Unexpected number of results"

instance (Typeable a, Typeable b, Typeable c) => TxType (UTxO a, UTxO b, UTxO c) where
  type Contract (UTxO a, UTxO b, UTxO c) = SmartContract (UTxORef a, UTxORef b, UTxORef c)

  txify (a, b, c) [] = [Some a, Some b, Some c]
  txify _ _  = error "txify: expecting no arguments to mkTx @(UTxO a, UTxO b)"

  atomic txfun args = Atomically txfun args cont
    where cont [Some (cast -> Just a), Some (cast -> Just b), Some (cast -> Just c)] = Done (a, b, c)
          cont _                                                                     = error "Unexpected number of results"

instance (Typeable a, TxType b) => TxType (UTxO a -> b) where
  type Contract (UTxO a -> b) = UTxORef a -> Contract b
  txify f (Some (cast -> Just input) : inputs) = txify (f input) inputs
  txify _ _ = error "txify: wrong argument type"

  atomic txfun args = \ ref -> atomic @b txfun (args ++ [Some ref])

tx :: forall a. TxType a => a -> Contract a
tx a = atomic @a (txify a) []

observe :: forall a. UTxORef a -> SmartContract a
observe ref = Observe ref Done

findUTxOs :: forall a. Typeable a => Address -> SmartContract [UTxORef a]
findUTxOs addr = FindUTxOs addr Done

new :: SmartContract (UTxORef a)
new = return NoUTxORef

-- TODO: build a little interpreter for this language
--
-- TODO:
-- In this model - how do you know what function
-- can be applied to what UTxO and how do we model
-- that failure in our modelling framework??
--
-- Maybe we need something like:
--  (fibTx :: Tx (UTxO (Int, Int) -> UTxO (Int, Int)), unfibTx :: Tx (UTxO (Int, Int) -> UTxO ())) <- bind (fib, unfib) "fib"
-- In the monad? This would be what creates
-- the script "fib" - no other functions are allowed to interact with "fib" UTxOs?
-- How do we compose functions that do several things at the same time? How do we deal
-- with the whole composition thing here?
--
-- It would be nice to write compostions that work on several scripts as just nicely
-- composed haskell functions. But if we need a notion of a `Tx` rather than a simple
-- function to act as the "only ok way to use 'fib'" then don't we miss some
-- compositionality? How do we use compositional functions that act across two "related"
-- scripts? They somehow need to be "bound" twice to two different script names?
--
-- We also still need to get the notion of "who can do what" in here somehow. How would that
-- work?
