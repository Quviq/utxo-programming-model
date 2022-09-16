{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Examples.LinearAuction where

import Prelude.Linear (($))
import Prelude hiding (($))

import LinearUTxOModel
import Value

import Data.Unrestricted.Linear

import Data.Group

{- The linear idea:

  We want to write "builder functions" that work on UTxOs to define contracts.
  However, we don't have a whole compiler back-end to use to turn these builder
  functions into actual validators. This is a problem because in order to emulate
  the chain we need to guarantee that our "pure functions transforming UTxO sets"
  (i.e. our view of transactions) are actually in comformance with the validators
  *implied by our builders*.

  Barring some magic the only real way to do this is to ensure that the only way
  to actually transform the UTxOs belonging to a given contract is to *use the
  actual builder functions* to build transactions that are value-preserving.

  Now, one could imagine that the way to do this is to only expose the builder
  functions as the destructors of "auction" or "escrow" UTxOs (using some
  type-index + module system magic and not allowing you to pattern-match on
  UTxOs). However, *this is not enough* because one might write a function like:

  cheat auctionUTxO = UTxO myWallet (valueOf auctionUTxO) ()

  that would be able to "steal" the money from the auction contract.
  You might think that you could avoid this by not exposing the `valueOf` function
  (which anyway isn't a workable solution!). But you still have the same problem!

  cheat auctionUTxO = UTxO myWallet (ada 10) ()

  This function would steal the value of the auction contract if and
  only if it happens to be 10 ada.

  The way around this is to use linear types to *guarantee* that any
  transaction (pure function from utxos to utxos) actually consumes
  the input transactions using the exposed builder functions!

  If we force transactions to be of a type like:

  cheat :: UTxO "auction" AuctionData -o UTxO SomeWallet ()

  we don't have this problem any more!

  Now, there are lots of details to work out but that's later Ulf and Max's problem.
  Now it's playtime.

  One more thing!

  To use a `UTxO SomeWallet ()` one would need something like a function of type
  `UTxO SomeWallet () -o ()` that would acutally consume the UTxO (you can't pattern
  match on UTxOs remember!). Now, no such function should exist in general but you could
  have a monadic action in the `SmartContract` monad that gives you this - something like:

  signedBy wallet1 $ \ destructor :: UTxO SomeWallet () -o () -> myTxCode destructor

  where `destructor` here would check that the input UTxO actually belongs to Wallet 1.

  ---

  One of many questions that remains to be answered is what the implications of this are
  when we go to our own stand-alone language? Clearly (?) we still need some form of linear
  type system - maybe it can be cleverererrere than Haskell's? Anyway, now it's *really*
  play time!

-}

data AuctionData =
  AuctionData { winner     :: Address
              , owner      :: Address
              , winningBid :: Value
              }

spendFrom :: Value -> UTxO a %1 -> UTxO a
spendFrom v (UTxO a v' d)
  | v == v'   = NoUTxO
  | otherwise = UTxO a (v' ~~ v) d
spendFrom _ utxo = failTx utxo "spendFrom NoUTxO"

setup :: Value -> UTxO () %1 -> (UTxO AuctionData, UTxO ())
setup value utxo =
  let' (addressOf utxo) $ \ (Ur addr, utxo) -> (setupInner value addr, spendFrom value utxo)

setupInner :: Value -> Address -> UTxO AuctionData
setupInner value owner = UTxO "auction" value datum
  where datum = AuctionData { winner     = owner
                            , owner      = owner
                            , winningBid = mempty
                            }

bid :: UTxO AuctionData
 %1 -> Value
    -> UTxO ()
 %1 -> (UTxO AuctionData, UTxO (), UTxO ())
bid auctionUTxO value utxo =
  let' (addressOf utxo)                  $ \ (Ur addr, utxo) ->
  let' (bidInner auctionUTxO value addr) $ \ (auctionUTxO', paybackUTxO) ->
  (auctionUTxO', paybackUTxO, spendFrom value utxo)

bidInner :: UTxO AuctionData
      %1 -> Value
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
bidInner utxo _ _ = failTx utxo "bidInner: invalid input"
