{-# LANGUAGE NoImplicitPrelude #-}
module Examples.Auction.Trusted
  ( Auction
  , AuctionData(..)
  , bidInner
  , settleInner
  ) where

import Prelude.Linear (($))
import Prelude hiding (($))

import UTxO.Model

import Control.DeepSeq
import GHC.Generics
import Data.Unrestricted.Linear
import Data.Group

data AuctionData = AuctionData
  { winner       :: Address
  , auctionOwner :: PubKeyHash
  , winningBid   :: Value
  , forSale      :: Value
  } deriving stock (Show, Generic)
    deriving anyclass NFData

data Auction = Auction
  deriving stock (Show, Generic)
  deriving anyclass NFData

-- TODO: not happy with this yet - it's an
-- annoying consequence of the silly Haskell
-- embedding.
instance IsOwner Auction where
  fresh _ = Just Auction

own :: Signature Auction
own Auction = ()

bidInner :: UTxO Auction AuctionData
      %1 -> Value
         -> Address
         -> (UTxO Auction AuctionData, Maybe (UTxO AnyOwner ()))
bidInner utxo newBid bidder =
  let' (useUTxO utxo own) $ \ (Ur (addr, value, d@AuctionData{..})) ->
  if | adaOf winningBid < adaOf newBid ->
        let auctionUTxO = mkUTxO addr
                                 (value <> newBid <> invert winningBid)
                                 d{ winningBid = newBid
                                  , winner     = bidder }
            paybackUTxO
              | winningBid == mempty = Nothing
              | otherwise            = Just $ mkUTxO winner winningBid ()
        in (auctionUTxO, paybackUTxO)
     | otherwise -> failTx () "New bid too small"

settleInner :: Signature PubKeyOwner
            -> UTxO Auction AuctionData
         %1 -> (UTxO PubKeyOwner (), Maybe (UTxO AnyOwner ()))
settleInner sign utxo =
  let' (useUTxO utxo own) $ \ (Ur (_, value, AuctionData{..})) ->
  let' (checkSignature sign auctionOwner) $ \ () ->
  if | winningBid == mempty -> (mkPubKeyUTxO auctionOwner value, Nothing)
     | otherwise            -> (mkPubKeyUTxO auctionOwner winningBid,
                                Just $ mkUTxO winner (value <> invert winningBid) ())
