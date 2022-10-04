{-# LANGUAGE UndecidableInstances #-}
module UTxO.Model
  ( -- * Dealing with values
    module UTxO.Value
  -- * Writing Validators
  -- ** UTxOs
  , UTxO
  , PubKeyHash(..)
  , Address(..)
  -- ** Ownership
  , IsOwner(..)
  , PubKeyOwner
  , AnyOwner
  , Signature
  -- ** Consuming UTxOs
  , spendUTxO
  , matchUTxO
  , useUTxO
  , spendFromUTxO
  , castUTxO
  , addressOf
  -- ** Failing transactions
  , checkSignature
  , failTx
  -- ** Creating UTxOs
  , mkUTxO
  , mkScriptAddress
  , mkScriptUTxO
  , mkPubKeyUTxO
  -- ** Dealing with time
  , TrueTime
  , Time
  , lowerBound
  , upperBound
  -- * Writing smart contracts
  , SmartContract
  , UTxORef(..)
  , UTxORefs
  , ToUTxORefs
  , Transaction(..)
  , IsTx(..)
  , Inputs
  , Outputs
  , tx
  , withTime
  , withSignature
  , submit
  , lookupUTxO
  , index
  , findWalletUTxOWhere
  -- * Linearity helpers
  , let'
  ) where

import Prelude.Linear (($))
import Prelude hiding (($))

import Control.Monad

import Data.Group
import Data.Unrestricted.Linear
import Data.Typeable

import UTxO.Value
import UTxO.Types
import UTxO.Trusted hiding (withTime, withSignature)
import UTxO.Trusted qualified

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

spendFromUTxO :: IsOwner owner
              => Signature owner
              -> Value
              -> UTxO owner datum
           %1 -> Maybe (UTxO owner datum)
spendFromUTxO sign v utxo =
  let' (useUTxO utxo sign) $ \ (Ur a, Ur v', Ur d) ->
  if | v' == v   -> Nothing
     | otherwise -> Just $ mkUTxO a (v' <> invert v) d

mkScriptAddress :: forall (owner :: *). Typeable owner => Address
mkScriptAddress = Script $ typeRep @[] @owner []

mkScriptUTxO :: forall owner datum. IsOwner owner => Value -> datum -> UTxO owner datum
mkScriptUTxO = mkUTxO (mkScriptAddress @owner)

mkPubKeyUTxO :: PubKeyHash -> Value -> UTxO PubKeyOwner ()
mkPubKeyUTxO hash value = mkUTxO (Wallet hash) value ()

checkSignature :: Signature PubKeyOwner -> PubKeyHash -> ()
checkSignature sign hash = case fresh $ Wallet hash of
  Nothing    -> failTx () "The impossible happened"
  Just owner -> sign owner

newtype Transaction t = Transaction { unTransaction :: TxRep (Inputs t) (Outputs (ResultOf t)) }

tx :: IsTx t => t -> Transaction t
tx t = Transaction $ transform $ txFun t

withSignature :: PubKeyHash -> (Signature PubKeyOwner -> Transaction t) -> Transaction t
withSignature pkh tx = Transaction $ UTxO.Trusted.withSignature pkh (\ sign -> unTransaction (tx sign) )

withTime :: Time -> Time -> (TrueTime -> Transaction t) -> Transaction t
withTime t0 t1 tx = Transaction $ UTxO.Trusted.withTime t0 t1 (\ tt -> unTransaction (tx tt))

findWalletUTxOWhere :: PubKeyHash
                    -> (Value -> Bool)
                    -> SmartContract (UTxORef PubKeyOwner ())
findWalletUTxOWhere pkh p = do
  let isValidUTxO ref = do
        Just (_, v, _) <- lookupUTxO ref
        return (p v)
  utxo : _ <- filterM isValidUTxO =<< index (Wallet pkh)
  return utxo

submit :: forall t. IsTx t
       => Transaction t
       -> SmartContractTx t
submit tx = submitInternal @t @'[] tx Nil

type family SmartContractTx t where
  SmartContractTx (UTxO owner datum %1 -> t) = UTxORef owner datum -> SmartContractTx t
  SmartContractTx t                          = SmartContract (ToUTxORefs t)

type family ToUTxORefs t where
  ToUTxORefs ()                 = ()
  ToUTxORefs (UTxO o d)         = UTxORef o d
  ToUTxORefs (Maybe (UTxO o d)) = Maybe (UTxORef o d)
  ToUTxORefs (a, b)             = (ToUTxORefs a, ToUTxORefs b)
  ToUTxORefs (a, b, c)          = (ToUTxORefs a, ToUTxORefs b, ToUTxORefs c)
  ToUTxORefs (a, b, c, d)       = (ToUTxORefs a, ToUTxORefs b, ToUTxORefs c, ToUTxORefs d)

type family ResultOf a where
  ResultOf (a %1 -> b) = ResultOf b
  ResultOf (a -> b)    = ResultOf b
  ResultOf a           = a

-- TODO: Outputs might be a bad name
type family Outputs a :: [*] where
  Outputs ()                         = '[]
  Outputs (UTxO owner datum)         = (owner, datum) : '[]
  Outputs (Maybe (UTxO owner datum)) = (owner, datum) : '[]
  Outputs (a, b)                     = Append (Outputs a) (Outputs b)
  Outputs (a, b, c)                  = Append (Outputs a) (Append (Outputs b) (Outputs c))
  Outputs (a, b, c, d)               = Append (Outputs a) (Append (Outputs b) (Append (Outputs c) (Outputs d)))

class IsTypeList (Outputs a) => Result a where
  toResult   :: a %1 -> MaybeUTxOs (Outputs a)
  fromResult :: forall x. MaybeUTxORefs (Append (Outputs a) x) -> (ToUTxORefs a, MaybeUTxORefs x)

instance Result (UTxO owner datum) where
  toResult utxo = Cons (MaybeF2 $ Just utxo) Nil

  fromResult (Cons (MaybeF2 (Just utxoref)) x) = (utxoref, x)
  fromResult (Cons (MaybeF2 Nothing) _)        = error "The impossible happened: fromResult @(UTxO owner datum) encountered Nothing"

instance Result (Maybe (UTxO owner datum)) where
  toResult mutxo = Cons (MaybeF2 mutxo) Nil

  fromResult (Cons (MaybeF2 mutxoref) x) = (mutxoref, x)

instance forall a b. (IsTypeList (Append (Outputs a) (Outputs b)), Result a, Result b) => Result (a, b) where
  toResult (a, b) = tList2Append (toResult a) (toResult b)

  fromResult :: forall x. MaybeUTxORefs (Append (Outputs (a, b)) x) -> (ToUTxORefs (a, b), MaybeUTxORefs x)
  fromResult xs
    | Refl <- appendAssocProof @(Outputs a) @(Outputs b) @x =
      let (a, xs') = fromResult @a @(Append (Outputs b) x) xs
          (b, xs'') = fromResult @b xs'
      in ((a, b), xs'')

instance (IsTypeList (Append (Outputs a) (Append (Outputs b) (Outputs c))), Result a, Result b, Result c) => Result (a, b, c) where
  toResult (a, b, c) = tList2Append (toResult a) (tList2Append (toResult b) (toResult c))

  fromResult :: forall x. MaybeUTxORefs (Append (Outputs (a, b, c)) x) -> (ToUTxORefs (a, b, c), MaybeUTxORefs x)
  fromResult xs
    | Refl <- appendAssocProof @(Outputs b) @(Outputs c) @x
    , Refl <- appendAssocProof @(Outputs a) @(Append (Outputs b) (Outputs c)) @x =
      let (a, xs') = fromResult @a @_ xs
          (b, xs'') = fromResult @b @_ xs'
          (c, xs''') = fromResult @c @_ xs''
      in ((a, b, c), xs''')

type family Inputs a :: [*] where
  Inputs (UTxO owner datum %1 -> a) = (owner, datum) : Inputs a
  Inputs _                          = '[]

type family (is :: [*]) :-> (t :: *) :: * where
  '[] :-> t = t
  ((owner, datum) : is) :-> t = UTxO owner datum %1 -> (is :-> t)

inputsAppendProof :: forall is t. Inputs (is :-> t) :~: Append is (Inputs t)
inputsAppendProof = undefined

resultFunProof :: forall is t. ResultOf (is :-> t) :~: ResultOf t
resultFunProof = undefined

funAppendDecomposeProof :: forall is is' t. (is :-> (is' :-> t)) :~: (Append is is' :-> t)
funAppendDecomposeProof = undefined

class Result (ResultOf t) => IsTx t where
  txFun :: t %1 -> UTxOs (Inputs t) %1 -> MaybeUTxOs (Outputs (ResultOf t))
  submitInternal :: forall is. (IsTypeList is, Inputs (is :-> t) ~ Append is (Inputs t), ResultOf (is :-> t) ~ ResultOf t)
                 => Transaction (is :-> t)
                 -> UTxORefs is
                 -> SmartContractTx t

instance {-# OVERLAPPABLE #-} ( Inputs t ~ '[]
                              , ResultOf t ~ t
                              , Result t
                              , SmartContractTx t ~ SmartContract (ToUTxORefs t))
                              => IsTx t where
  txFun r = (\ Nil -> toResult r)

  submitInternal :: forall is. (IsTypeList is, Inputs (is :-> t) ~ Append is (Inputs t), ResultOf (is :-> t) ~ ResultOf t)
                 => Transaction (is :-> t)
                 -> UTxORefs is
                 -> SmartContractTx t
  submitInternal (Transaction tx) irefs
    | Refl <- appendRightUnitProof @(Outputs t)
    , Refl <- appendRightUnitProof @is = do
        orefs <- submitTx tx irefs
        let (refs, _) = fromResult @(ResultOf t) @'[] orefs
        return refs

instance {-# OVERLAPPING #-} (t ~ (UTxO owner datum %1 -> b), IsTx b) => IsTx (UTxO owner datum %1 -> b) where
  txFun f (Cons utxo args) = txFun (f utxo) args

  submitInternal :: forall is. (IsTypeList is, Inputs (is :-> t) ~ Append is (Inputs t), ResultOf (is :-> t) ~ ResultOf t)
                 => Transaction (is :-> t)
                 -> UTxORefs is
                 -> SmartContractTx t
  submitInternal tx irefs i
    | Dict <- appendTypeListDict @is @'[(owner, datum)]
    , Refl <- inputsAppendProof @(Append is '[(owner, datum)]) @b
    , Refl <- resultFunProof @(Append is '[(owner, datum)]) @b
    , Refl <- funAppendDecomposeProof @is @'[(owner, datum)] @b
    = submitInternal @b tx (tList2Append irefs (Cons i Nil))
