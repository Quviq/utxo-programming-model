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
  in (mkScriptUTxO value datum, spendFromUTxO sign value utxo)

bid :: IsOwner owner
    => Signature owner
    -> Value
    -> UTxO Auction AuctionData
 %1 -> UTxO owner ()
 %1 -> ( UTxO Auction AuctionData
       , Maybe (UTxO AnyOwner ())
       , Maybe (UTxO owner ()))
bid sign bid auctionUTxO utxo =
  let' (addressOf utxo)                $ \ (Ur addr, utxo) ->
  let' (bidInner auctionUTxO bid addr) $ \ (auctionUTxO', paybackUTxO) ->
  (auctionUTxO', paybackUTxO, spendFromUTxO sign bid utxo)

runSetupTx :: PubKeyHash
           -> Value
           -> SmartContract (UTxORef Auction AuctionData)
runSetupTx pkh value = do
  utxo <- findWalletUTxOWhere pkh (value `leq`)
  let setupTx = withSignature pkh $ \sig -> tx $ setup sig value
  fst <$> submit setupTx utxo

runBidTx :: PubKeyHash
         -> Value
         -> UTxORef Auction AuctionData
         -> SmartContract (UTxORef Auction AuctionData)
runBidTx pkh myBid auctionUTxO = do
  utxo <- findWalletUTxOWhere pkh (myBid `leq`)
  let bidTx = withSignature pkh $ \sign -> tx $ bid sign myBid
  (auctionUTxO, _, _) <- submit bidTx auctionUTxO utxo
  return auctionUTxO
