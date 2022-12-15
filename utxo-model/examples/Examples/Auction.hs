{-# LANGUAGE NoImplicitPrelude #-}
module Examples.Auction where

import Prelude.Linear (($))
import Prelude hiding (($))

import UTxO.Model

import Data.Unrestricted.Linear

import Examples.Auction.Trusted

setup :: Signature PubKeyOwner
      -> PubKeyHash
      -> Value
      -> UTxO PubKeyOwner ()
   %1 -> (UTxO Auction AuctionData, Maybe (UTxO PubKeyOwner ()))
setup sign pkh value utxo =
  let datum = AuctionData { winner       = Wallet pkh
                          , auctionOwner = pkh
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
runSetupTx pkh value = onWallet pkh $ do
  utxo <- findWalletUTxOWhere pkh (value `leq`)
  let setupTx = withSignature pkh $ \sig -> tx $ setup sig pkh value
  fst <$> submitTx setupTx utxo

runBidTx :: PubKeyHash
         -> Value
         -> UTxORef Auction AuctionData
         -> SmartContract (UTxORef Auction AuctionData)
runBidTx pkh myBid auctionUTxO = onWallet pkh $ do
  utxo <- findWalletUTxOWhere pkh (myBid `leq`)
  let bidTx = withSignature pkh $ \sign -> tx $ bid sign myBid
  (auctionUTxO, _, _) <- submitTx bidTx auctionUTxO utxo
  return auctionUTxO
