{-# LANGUAGE NoImplicitPrelude #-}
module Examples.Auction where

import Prelude.Linear (($))
import Prelude hiding (($))

import UTxO.Model

import Data.Unrestricted.Linear

import Examples.Auction.Trusted

setup :: Signature PubKeyOwner
      -> Value
      -> UTxO PubKeyOwner ()
   %1 -> (UTxO Auction AuctionData, Maybe (UTxO PubKeyOwner ()))
setup sign value utxo =
  let' (addressOf utxo) $ \ (Ur (Wallet hash), utxo) ->
  let datum = AuctionData { winner       = Wallet hash
                          , auctionOwner = hash
                          , winningBid   = mempty
                          , forSale      = value
                          }
  in (mkScriptUTxO value datum, spendFrom sign value utxo)

bid :: IsOwner owner
    => Signature owner
    -> UTxO Auction AuctionData
 %1 -> UTxO owner ()
 %1 -> Value
    -> ( UTxO Auction AuctionData
       , Maybe (UTxO AnyOwner ())
       , Maybe (UTxO owner ()))
bid sign auctionUTxO utxo bid =
 let' (addressOf utxo)                $ \ (Ur addr, utxo) ->
 let' (bidInner auctionUTxO bid addr) $ \ (auctionUTxO', paybackUTxO) ->
 (auctionUTxO', paybackUTxO, spendFrom sign bid utxo)
