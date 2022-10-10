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
  , lookupUTxO
  , index
  , awaitTime
  , onWallet
  -- * Semantics
  , Chain(..)
  , runSmartContract
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Except

import Data.Typeable
import Data.Map (Map)
import Data.Map qualified as Map

import UTxO.Value
import UTxO.Types

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

-- TODO: is this safe? We need to write down how these owner tricks work!
coerceUTxORef :: UTxORef AnyOwner datum -> UTxORef owner datum
coerceUTxORef = UTxORef . getRef

type UTxORefs = TList2 UTxORef
type MaybeUTxORefs = TList2 (MaybeF2 UTxORef)

data TxRep inputs outputs where
  Transform :: (UTxOs inputs %1 -> MaybeUTxOs outputs)
            -> TxRep inputs outputs

  WithSignature :: PubKeyHash
                -> (Signature PubKeyOwner -> TxRep inputs outputs)
                -> TxRep inputs outputs

  WithTime :: Time
           -> Time
           -> (TrueTime -> TxRep inputs outputs)
           -> TxRep inputs outputs

data SmartContract a where
  Done :: a -> SmartContract a

  Submit :: TxRep inputs outputs
         -> UTxORefs inputs
         -> (MaybeUTxORefs outputs -> SmartContract a)
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

transform :: (UTxOs inputs %1 -> MaybeUTxOs outputs) -> TxRep inputs outputs
transform = Transform

withSignature :: PubKeyHash -> (Signature PubKeyOwner -> TxRep inputs outputs) -> TxRep inputs outputs
withSignature = WithSignature

withTime :: Time -> Time -> (TrueTime -> TxRep inputs outputs) -> TxRep inputs outputs
withTime = WithTime

submitTx :: TxRep inputs outputs -> UTxORefs inputs -> SmartContract (MaybeUTxORefs outputs)
submitTx tx is = Submit tx is Done

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
           => Proxy owner
           -> Address
           -> Value
           -> datum
           -> SomeUTxO

data EmulationState = EmulationState
  { _utxos         :: Map Int SomeUTxO
  , _stxos         :: Map Int SomeUTxO
  , _currentTime   :: Time
  , _currentWallet :: Maybe PubKeyHash
  }
makeLenses ''EmulationState

type Semantics = ExceptT String (State EmulationState)

runSubmitTx :: forall inputs outputs.
               TxRep inputs outputs
            -> UTxORefs inputs
            -> Semantics (MaybeUTxORefs outputs)
runSubmitTx tx inputs = case tx of
  Transform fun         -> _
  WithSignature pkh fun -> _
  WithTime t0 t1 fun    -> do
    t <- currentTime
    if t0 <= t && t <= t1
    then runSubmitTx (fun $ TrueTime t0 t1)
    -- TODO: implement this - ghc makes this harder than it looks unfortunately!
    -- We need to revamp the whole type directed machinery to make all this
    -- nice.
    else return $ allNothings @outputs

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
                             | (i, SomeUTxO @_ @datum' _ a _ d) <- utxoList
                             , a == addr
                             , Just Refl <- [eqT @datum @datum']
                             ]
      _ -> do
        runSmartContract $ c []

  Observe @owner @datum r c -> do
    mUTxO <- use $ utxos . at (getRef r)
    runSmartContract . c $ do
      SomeUTxO @owner' @datum' o a v d <- mUTxO
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
