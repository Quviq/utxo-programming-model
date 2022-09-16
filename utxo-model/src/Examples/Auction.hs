module Examples.Auction where

import UTxOModel
import Value

import Data.Group

data AuctionData =
  AuctionData { winner     :: Address
              , owner      :: Address
              , winningBid :: Value
              }

spendFrom :: Value -> UTxO a -> UTxO a
spendFrom v (UTxO a v' d)
  | v == v'   = NoUTxO
  | otherwise = UTxO a (v' ~~ v) d
spendFrom _ _ = error "spendFrom NoUTxO"

setup :: Value -> UTxO () -> (UTxO AuctionData, UTxO ())
setup value utxo = ( setupInner value (addressOf utxo)
                   , spendFrom value utxo)

setupInner :: Value -> Address -> UTxO AuctionData
setupInner value owner = UTxO "auction" value datum
  where datum = AuctionData { winner     = owner
                            , owner      = owner
                            , winningBid = mempty
                            }

bid :: UTxO AuctionData
    -> Value
    -> UTxO ()
    -> (UTxO AuctionData, UTxO (), UTxO ())
bid auctionUTxO value utxo =
  let (auctionUTxO', paybackUTxO) = bidInner auctionUTxO value (addressOf utxo)
  in (auctionUTxO', paybackUTxO, spendFrom utxo value)

bidInner :: UTxO AuctionData
         -> Value
         -> Address
         -> (UTxO AuctionData, UTxO ())
bidInner (UTxO "auction" v d@AuctionData{..}) newBid bidder
  | adaOf winningBid < adaOf newBid
    -- This is a design decision:
    --   You either require that a bidder put in enough ada to pay for
    --   their own UTxO to be returned, or you require that the new
    --   bidder do it. Either decision is fine, here we make the
    --   choice to require the bidder to self-fund.
  , adaOf newBid >= minAda (UTxO bidder newBid ()) =
    let auctionUTxO = UTxO "auction" ((v <> newBid) ~~ winningBid)
                                     d { winningBid = newBid
                                       , winner     = bidder }
        paybackUTxO
          | winningBid == mempty = NoUTxO
          | otherwise            = UTxO winner winningBid ()
    in (auctionUTxO, paybackUTxO)
  | otherwise = error "bidInner: newBid too small"
bidInner _ _ _ = error "bidInner: invalid input"