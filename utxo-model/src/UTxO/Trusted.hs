{-# LANGUAGE UndecidableInstances #-}
module UTxO.Trusted
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
  , Transaction
  , UTxORef(..)
  , IsTx(..)
  , SubmitType
  , tx
  , withSignature
  , withTime
  , submitTx
  , lookupUTxO
  , index
  , awaitTime
  , onWallet
  -- * Semantics
  , EmulationState(..)
  , runSmartContract
  ) where

import Control.Lens hiding (index)
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.DeepSeq
import Control.Exception

import System.IO.Unsafe

import GHC.Generics (Generic)

import Data.Maybe
import Data.Typeable
import Data.Map (Map)
import Data.Map qualified as Map

import UTxO.Value

import Unsafe.Linear qualified as Unsafe
import Data.Unrestricted.Linear hiding (lift)

newtype PubKeyHash = PubKeyHash { unPubKeyHash :: Int }
  deriving (Ord, Eq, Show)
  deriving NFData via Int

data Address where
  Script :: TypeRep -> Address
  Wallet :: PubKeyHash -> Address
  deriving (Ord, Eq, Show, Generic, NFData)

class (NFData owner, Typeable owner) => IsOwner owner where
  fresh :: Address -> Maybe owner

data AnyOwner where
  AnyOwner :: Address -> AnyOwner
  deriving (Generic, NFData)

instance IsOwner AnyOwner where
  fresh = Just . AnyOwner

data PubKeyOwner = PubKeyOwner PubKeyHash
  deriving (Generic, NFData)

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
  deriving (Ord, Eq, Show, Generic, NFData)

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
-- tx :: (UTxOs n %1 -> UTxOs (Succ n)) -> TransactionType

newtype UTxORef owner datum = UTxORef { getRef :: Int }

data Transaction t where
  Transform :: t -> Transaction t

  WithSignature :: PubKeyHash
                -> (Signature PubKeyOwner -> Transaction t)
                -> Transaction t

  WithTime :: Time
           -> Time
           -> (TrueTime -> Transaction t)
           -> Transaction t

data SmartContract a where
  Done :: a -> SmartContract a

  Submit :: IsTx t
         => Transaction t
         -> InputRefs t
         -> (OutputRefs t -> SmartContract a)
         -> SmartContract a

  UTxOsAt :: forall (owner :: *) (datum :: *) (a :: *).
             (Typeable datum, IsOwner owner)
          => Address
          -> ([UTxORef owner datum] -> SmartContract a)
          -> SmartContract a

  Observe :: forall (owner :: *) (datum :: *) (a :: *).
             (Typeable owner, Typeable datum)
          => UTxORef owner datum
          -> (Maybe (Address, Value, datum) -> SmartContract a)
          -> SmartContract a

  Fail :: String
       -> SmartContract a

  AwaitTime :: Time
            -> SmartContract a
            -> SmartContract a

  OnWallet :: PubKeyHash
           -> SmartContract a
           -> SmartContract a

type family InputRefs t where
  InputRefs (UTxO owner datum %1 -> t) = (UTxORef owner datum, InputRefs t)
  InputRefs t                          = ()

type family Inputs t where
  Inputs (UTxO owner datum %1 -> t) = (UTxO owner datum, Inputs t)
  Inputs t                          = ()

type family Output t where
  Output (UTxO owner datum %1 -> t) = Output t
  Output t                          = t

type family SubmitType t where
  SubmitType (UTxO owner datum %1 -> t) = UTxORef owner datum -> SubmitType t
  SubmitType t                          = SmartContract (Refs t)

class IsOutput (Output t) => IsTx t where
  traverseInputs :: Monad m
                 => (forall owner datum. (Typeable owner, Typeable datum)
                                      => UTxORef owner datum
                                      -> m (Maybe (UTxO owner datum)))
                 -> InputRefs t -> m (Maybe (Inputs t))
  txFun      :: t %1 -> Inputs t %1 -> Output t
  submitTx'  :: (IsTx top, OutputRefs top ~ OutputRefs t)
             => Transaction top
             -> (InputRefs t -> InputRefs top)
             -> SubmitType t

type OutputRefs t = Refs (Output t)

class (NFData t, Output t ~ t, Inputs t ~ (), InputRefs t ~ (), SubmitType t ~ SmartContract (Refs t)) => IsOutput t where
  type Refs t :: *
  traverseOutput :: Applicative m
                 => (forall owner datum. (Typeable owner, Typeable datum)
                                      => UTxO owner datum
                                      -> m (UTxORef owner datum))
                 -> t -> m (Refs t)

instance {-# OVERLAPPING #-} (Typeable owner, Typeable datum, IsTx t) => IsTx (UTxO owner datum %1 -> t) where
  traverseInputs f (ref, refs) = do
    mutxo <- f ref
    mutxos <- traverseInputs @t f refs
    return $ (,) <$> mutxo <*> mutxos
  txFun f (input, inputs) = txFun (f input) inputs
  submitTx' rep k input = submitTx' @t rep (k . (input,))

instance {-# OVERLAPPABLE #-} IsOutput t => IsTx t where
  traverseInputs _ () = pure $ Just ()
  txFun t () = t
  submitTx' rep k = Submit rep (k ()) Done

instance (NFData owner, NFData datum, Typeable owner, Typeable datum) => IsOutput (UTxO owner datum) where
  type Refs (UTxO owner datum) = UTxORef owner datum
  traverseOutput f = f

instance IsOutput t => IsOutput (Maybe t) where
  type Refs (Maybe t) = Maybe (Refs t)
  traverseOutput f = traverse (traverseOutput f)

instance IsOutput t => IsOutput [t] where
  type Refs [t] = [Refs t]
  traverseOutput f = traverse (traverseOutput f)

instance (IsOutput a, IsOutput b) => IsOutput (a, b) where
  type Refs (a, b) = (Refs a, Refs b)
  traverseOutput f (a, b) = (,) <$> traverseOutput f a <*> traverseOutput f b

instance (IsOutput a, IsOutput b, IsOutput c) => IsOutput (a, b, c) where
  type Refs (a, b, c) = (Refs a, Refs b, Refs c)
  traverseOutput f (a, b, c) = (,,) <$> traverseOutput f a <*> traverseOutput f b <*> traverseOutput f c

tx :: t -> Transaction t
tx = Transform

withSignature :: PubKeyHash -> (Signature PubKeyOwner -> Transaction t) -> Transaction t
withSignature = WithSignature

withTime :: Time -> Time -> (TrueTime -> Transaction t) -> Transaction t
withTime = WithTime

submitTx :: forall t. IsTx t => Transaction t -> SubmitType t
submitTx rep = submitTx' @t rep id

index :: forall (owner :: *) (datum :: *). (Typeable datum, IsOwner owner)
      => Address
      -> SmartContract [UTxORef owner datum]
index addr = UTxOsAt addr Done

lookupUTxO :: forall (owner :: *) (datum :: *).
              (Typeable owner, Typeable datum)
           => UTxORef owner datum
           -> SmartContract (Maybe (Address, Value, datum))
lookupUTxO ref = Observe ref Done

awaitTime :: Time -> SmartContract ()
awaitTime t = AwaitTime t (Done ())

onWallet :: PubKeyHash -> SmartContract a -> SmartContract a
onWallet = OnWallet

instance Functor SmartContract where
  fmap = liftM

instance Applicative SmartContract where
  pure = Done
  (<*>) = ap

instance Monad SmartContract where
  Done a         >>= k = k a
  Submit tx is c >>= k = Submit tx is (c >=> k)
  UTxOsAt a c    >>= k = UTxOsAt a (c >=> k)
  Observe r c    >>= k = Observe r (c >=> k)
  Fail s         >>= _ = Fail s
  AwaitTime t c  >>= k = AwaitTime t (c >>= k)
  OnWallet pkh c >>= k = OnWallet pkh (c >>= k)

instance MonadFail SmartContract where
  fail = Fail

data SomeUTxO where
  SomeUTxO :: forall (owner :: *) (datum :: *).
              (Typeable owner, Typeable datum)
           => owner
           -> Address
           -> Value
           -> datum
           -> SomeUTxO

unwrapUTxO :: (Typeable owner, Typeable datum) => SomeUTxO -> Maybe (UTxO owner datum)
unwrapUTxO (SomeUTxO owner addr val datum) = cast (UTxO owner addr val datum)

data EmulationState = EmulationState
  { _utxos         :: Map Int SomeUTxO
  , _stxos         :: Map Int SomeUTxO
  , _currentTime   :: Time
  , _currentWallet :: Maybe PubKeyHash
  , _nextRef       :: Int
  }
makeLenses ''EmulationState

type Semantics = ExceptT String (State EmulationState)

freshRef :: Semantics Int
freshRef = do
  r <- use nextRef
  nextRef += 1
  pure r

forceMaybe :: NFData a => a -> Maybe a
forceMaybe a =
  unsafePerformIO . flip (catch @SomeException) (const $ pure Nothing) . fmap Just $ a `deepseq` return a

runSubmitTx :: forall t.
               IsTx t
            => Transaction t
            -> InputRefs t
            -> Semantics (OutputRefs t)
runSubmitTx tx inputRefs = case tx of
  Transform fun         -> do
    st <- get
    let consumeInput (UTxORef i) = runMaybeT $ do
          sUTxO@(SomeUTxO _ _ val _) <- MaybeT $ use $ utxos . at i
          utxos . at i .= Nothing
          stxos . at i .= Just sUTxO
          tell val
          MaybeT . pure $ unwrapUTxO sUTxO
    (mInputs, inVal) <- runWriterT
                       $ traverseInputs @t consumeInput inputRefs
    case mInputs of
      Nothing -> throwE "Bad refs"
      Just inputs -> do
        let out = txFun @t fun inputs
            allocateRef (UTxO owner addr val datum) = do
              i <- lift freshRef
              utxos %= Map.insert i (SomeUTxO owner addr val datum)
              tell val
              pure $ UTxORef i
        (outRefs, outVal) <- runWriterT $ traverseOutput @(Output t) allocateRef out
        when (isNothing (forceMaybe out)) $ do
          put st
          throwE $ "Transaction failed"
        when (inVal /= outVal) $ do -- TODO: fees + minAda
          put st -- rollback state to before transaction
          throwE $ "inVal: " ++ show inVal ++ " /= outVal: " ++ show outVal
        pure outRefs
  WithSignature _pkh _fun -> error "TODO"
  WithTime t0 t1 fun    -> do
    t <- use currentTime
    if t0 <= t && t <= t1
    then runSubmitTx (fun $ TrueTime t0 t1) inputRefs
    else throwE "Not in correct time slot"

runSmartContract :: SmartContract a -> Semantics a
runSmartContract sc = case sc of
  Done a -> do
    return a

  Submit tx is c -> do
    a <- runSubmitTx tx is
    runSmartContract (c a)

  UTxOsAt @owner @datum addr c -> do
    let mOwner = fresh @owner addr
    case mOwner of
      Just owner | isAddressOf addr owner -> do
        utxoList <- use $ utxos . to Map.toList
        runSmartContract $ c [ UTxORef i
                             | (i, SomeUTxO @_ @datum' _ a _ _d) <- utxoList
                             , a == addr
                             , Just Refl <- [eqT @datum @datum']
                             ]
      _ -> do
        runSmartContract $ c []

  Observe @owner @datum r c -> do
    mUTxO <- use $ utxos . at (getRef r)
    runSmartContract . c $ do
      SomeUTxO @owner' @datum' _ a v d <- mUTxO
      Refl                             <- eqT @owner @owner'
      Refl                             <- eqT @datum @datum'
      return (a, v, d)

  Fail s -> do
    throwE s

  AwaitTime t c -> do
    currentTime %= max t
    runSmartContract c

  OnWallet pkh c -> do
    wallet <- use currentWallet
    a <- case wallet of
      Just pkh'
        | pkh /= pkh' ->
          throwE $ "Trying to run on wallet "
                 ++ show pkh
                 ++ " inside a call to onWallet on wallet "
                 ++ show pkh'
      _ -> do
        currentWallet .= Just pkh
        runSmartContract c
    currentWallet .= wallet
    return a
