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
  } deriving Generic
    deriving anyclass NFData

data Auction = Auction deriving Generic
                       deriving anyclass NFData

own :: Signature Auction
own Auction = ()

bidInner :: UTxO Auction AuctionData
      %1 -> Value
         -> Address
         -> (UTxO Auction AuctionData, Maybe (UTxO AnyOwner ()))
bidInner utxo newBid bidder =
  let' (useUTxO utxo own) $ \ (Ur addr, Ur value, Ur d@AuctionData{..}) ->
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
  let' (useUTxO utxo own) $ \ (Ur _, Ur value, Ur AuctionData{..}) ->
  let' (checkSignature sign auctionOwner) $ \ () ->
  if | winningBid == mempty -> (mkPubKeyUTxO auctionOwner value, Nothing)
     | otherwise            -> (mkPubKeyUTxO auctionOwner winningBid,
                                Just $ mkUTxO winner (value <> invert winningBid) ())

-- TODO: not happy with this yet
instance IsOwner Auction where
  fresh _ = Just Auction
