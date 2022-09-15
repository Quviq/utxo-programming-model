module UTxOModel where

import Control.Monad

import Data.Typeable

import Value

data Some (f :: * -> *) where
  Some :: Typeable a => f a -> Some f

data UTxO a = NoUTxO
            | UTxO Value a
            deriving (Ord, Eq, Show)

newtype UTxORef a = UTxORef { unUTxORef :: Integer }

data SmartContract a where
  Done       :: a -> SmartContract a
  Atomically :: ([Some UTxO] -> [Some UTxO])
             -> [Some UTxORef]
             -> ([Some UTxORef] -> SmartContract c)
             -> SmartContract c

deriving instance Functor SmartContract

instance Applicative SmartContract where
  pure = Done
  (<*>) = ap

instance Monad SmartContract where
  Done a            >>= k = k a
  Atomically f as c >>= k = Atomically f as (c >=> k)

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
