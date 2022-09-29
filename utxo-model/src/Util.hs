{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Util where

import Prelude.Linear (($))
import Prelude hiding (($))

import Data.Group
import Data.Unrestricted.Linear
import Data.Typeable

import Value
import Types

import LinearUTxOModel

let' :: a %1 -> (a %1 -> b) %1 -> b
let' a f = f a

addressOf :: UTxO owner datum %1 -> (Ur Address, UTxO owner datum)
addressOf utxo = let' (matchUTxO utxo) $ \ (a, Ur _, Ur _, utxo) -> (a, utxo)

useUTxO :: UTxO owner datum
     %1 -> Signature owner
        -> (Ur Address, Ur Value, Ur datum)
useUTxO utxo sign =
  let' (matchUTxO utxo)      $ \ (a, v, d, utxo) ->
  let' (spendUTxO utxo sign) $ \ () ->
  (a, v, d)

spendFrom :: IsOwner owner
          => Signature owner
          -> Value
          -> UTxO owner datum
       %1 -> Maybe (UTxO owner datum)
spendFrom sign v utxo =
  let' (useUTxO utxo sign) $ \ (Ur a, Ur v', Ur d) ->
  if | v' == v   -> Nothing
     | otherwise -> Just $ mkUTxO a (v' <> invert v) d

mkScriptAddress :: Typeable owner => owner -> Address
mkScriptAddress = Script . typeOf

mkScriptUTxO :: forall owner datum. IsOwner owner => Value -> datum -> UTxO owner datum
mkScriptUTxO = mkUTxO (mkScriptAddress $ fresh @owner)

mkPubKeyUTxO :: PubKeyHash -> Value -> UTxO PubKeyOwner ()
mkPubKeyUTxO hash value = mkUTxO (Wallet hash) value ()

checkSignature :: Signature PubKeyOwner -> PubKeyHash -> ()
checkSignature sign hash = case fresh $ Wallet hash of
  Nothing    -> failTx () "The impossible happened"
  Just owner -> sign owner

tx :: Tx t => t -> TxRep (Inputs t) (Outputs t)
tx t = transform (txFun t)

submit :: TxRep inputs outputs -> UTxORefs inputs -> SmartContract (Unpacked outputs)
submit = undefined

-- * Don't look below this line, this is where all the dangerous type level stuff
--  that's ugly and unfinished lives!

type family Outputs a :: [*] where
  Outputs (a -> b)                   = Outputs b
  Outputs (a %1 -> b)                = Outputs b
  Outputs ()                         = '[]
  Outputs (UTxO owner datum)         = (owner, datum) : '[]
  Outputs (Maybe (UTxO owner datum)) = (owner, datum) : '[]
  Outputs (a, b)                     = Append (Outputs a) (Outputs b)
  Outputs (a, b, c)                  = Append (Outputs a) (Append (Outputs b) (Outputs c))
  Outputs (a, b, c, d)               = Append (Outputs a) (Append (Outputs b) (Append (Outputs c) (Outputs d)))

type family Unpacked (a :: [*]) :: * where
  Unpacked '[]                                    = ()
  Unpacked ((o, d) : '[])                         = Maybe (UTxORef o d)
  Unpacked ((o, d) : (o', d') : '[])              = (Maybe (UTxORef o d), Maybe (UTxORef o' d'))
  Unpacked ((o, d) : (o', d') : (o'', d'') : '[]) = (Maybe (UTxORef o d), Maybe (UTxORef o' d'), Maybe (UTxORef o'' d''))
  Unpacked ((o, d) : (o', d') : (o'', d'') : tys) = (Maybe (UTxORef o d), Maybe (UTxORef o' d'), Maybe (UTxORef o'' d''), Unpacked tys)

class Unpack (a :: [*]) where
  unpack :: MaybeUTxORefs a -> Unpacked a

instance Unpack '[] where
  unpack _ = ()

instance Unpack ((o, d) : '[]) where
  unpack (Cons m _) = unMaybeF2 m

instance Unpack ((o, d) : (o', d') : '[]) where
  unpack (Cons m (Cons m' _)) = (unMaybeF2 m, unMaybeF2 m')

instance Unpack ((o, d) : (o', d') : (o'', d'') : '[]) where
  unpack (Cons m (Cons m' (Cons m'' _))) = (unMaybeF2 m, unMaybeF2 m', unMaybeF2 m'')

instance Unpack (a : as) => Unpack ((o, d) : (o', d') : (o'', d'') : (a : as)) where
  unpack (Cons m (Cons m' (Cons m'' ms))) = (unMaybeF2 m, unMaybeF2 m', unMaybeF2 m'', unpack ms)

class Result a where
  toResult :: a %1 -> MaybeUTxOs (Outputs a)

instance Result (UTxO owner datum) where
  toResult utxo = Cons (MaybeF2 $ Just utxo) Nil

instance Result (Maybe (UTxO owner datum)) where
  toResult mutxo = Cons (MaybeF2 mutxo) Nil

instance (Result a, Result b) => Result (a, b) where
  toResult (a, b) = tList2Append (toResult a) (toResult b)

instance (Result a, Result b, Result c) => Result (a, b, c) where
  toResult (a, b, c) = tList2Append (toResult a) (tList2Append (toResult b) (toResult c))

type family Inputs a :: [*] where
  Inputs (UTxO owner datum %1 -> a) = (owner, datum) : Inputs a
  Inputs _                          = '[]

class Tx a where
  txFun :: a %1 -> UTxOs (Inputs a) %1 -> MaybeUTxOs (Outputs a)

instance (Inputs a ~ '[], Result a) => Tx a where
  txFun r = (\ Nil -> toResult r)

instance Tx b => Tx (UTxO owner datum %1 -> b) where
  txFun f (Cons utxo args) = txFun (f utxo) args


