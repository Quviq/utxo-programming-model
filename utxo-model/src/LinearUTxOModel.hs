{-# LANGUAGE LinearTypes #-}
module LinearUTxOModel where

import Data.String

import Value

import Unsafe.Linear qualified as Unsafe
import Data.Unrestricted.Linear

data Address = Script String
             | Wallet Int
             deriving (Ord, Eq, Show)

instance IsString Address where
  fromString = Script

data UTxO a where
  NoUTxO :: UTxO a
  UTxO   :: Address -> Value -> a -> UTxO a
  deriving (Ord, Eq, Show)

addressOf :: UTxO a %1 -> (Ur Address, UTxO a)
addressOf NoUTxO       = error "addressOf"
addressOf (UTxO a v d) = (Ur a, UTxO a v d)

let' :: a %1 -> (a %1 -> b) %1 -> b
let' a f = f a

minAda :: UTxO a -> Integer
minAda _ = 2_000_000

failTx :: a %1 -> String -> b
failTx = Unsafe.toLinear (const error)
