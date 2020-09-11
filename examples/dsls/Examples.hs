{-# OPTIONS_GHC -Wno-unused-imports #-}

module Examples where

import Marlowe
import Sim
import Static

-- advantages of smart contracts:
--
-- 1. Avoid ambiguity of contracts written in natural language: make meaning of contract precise
-- 2. Automatically enforce the contract without human intervention (on the blockchain).
--    In the "real world", contracts can be enforced by the legal system (courts, police, fines, prison...)
--    On a blockchain, this is not possible.
--
--    For example, in an auction, if a bidder makes a bid and wins the auction, he can be forced (by a court)
--    to actually pay what he promised afterwards.
--    But in an online auction on the blockchain, using a smart contract, that won't work. So instead, bidders should
--    for example make deposits before the auction even starts.

-- Alice sends Bob 100 ada.
aliceToBobFix :: Contract
aliceToBobFix =
    When
        [ Case (Deposit "bob" "alice" (Constant 100) ada) Close
        ]
        (Slot 10)
        Close

-- Write a Marlowe contract where Alice pays an arbitrary amount between 100 and 200 ada to Bob.
aliceToBobChoice :: Contract
aliceToBobChoice =
    When
        [ Case (Choice (ChoiceId "amountToSend" "alice") [Bound 100 200]) $
            When
                [ Case (Deposit "bob" "alice" (ChoiceValue (ChoiceId "amountToSend" "alice")) ada) Close
                ]
                (Slot 20)
                Close
        ]
        (Slot 10)
        Close

-- Write a "lottery" Marlowe contract: Participants all deposit a fixed amount of ada, and then "luck" decides who of them
-- gets all the money. (We model "luck" as yet another participant.)

lottery2 :: Contract
lottery2 =
    When
        [ Case (Deposit luck "alice" fee ada) $
            When
                [ Case (Deposit luck "bob" fee ada) bothHavePaidTheirFee
                ]
                (Slot 10)
                (Pay luck (Party "alice") fee ada Close)
        , Case (Deposit luck "bob" fee ada) $
            When
                [ Case (Deposit luck "alice" fee ada) bothHavePaidTheirFee
                ]
                (Slot 10)
                (Pay luck (Party "bob") fee ada Close)
        ]
        (Slot 10)
        Close
  where
    fee :: Value
    fee = Constant 100

    luck :: AccountId
    luck = "luck"

    bothHavePaidTheirFee :: Contract
    bothHavePaidTheirFee =
        When
            [ Case (Choice (ChoiceId "aliceOrBob" "luck") [Bound 0 0])
                (Pay luck (Party "alice") (AvailableMoney luck ada) ada Close)
            , Case (Choice (ChoiceId "aliceOrBob" "luck") [Bound 1 1])
                (Pay luck (Party "bob") (AvailableMoney luck ada) ada Close)
            ]
            (Slot 15)
            ( Pay luck (Party "alice") fee ada $
              Pay luck (Party "bob") fee ada   $
              Close)

lottery :: Amount -> Token -> [String] -> Contract
lottery _         _     []          = Close
lottery _         _     [_]         = Close
lottery feeAmount token playerNames = go [] playerNames
  where
    go :: [String] -- players who have paid
       -> [String] -- players who have not paid
       -> Contract
    go paid []      = payingOfFeesDone paid
    go paid notPaid =
        When (map deposit notPaid) (Slot 10) $
        payingOfFeesDone paid
      where
        deposit :: String -> Case
        deposit n =
            Case (Deposit "luck" n (Constant feeAmount) token) $
            go (n : paid) [n' | n' <- notPaid, n' /= n]

    payingOfFeesDone :: [String] -> Contract -- gets the names of those that paid their fees
    payingOfFeesDone []  = Close
    payingOfFeesDone [n] = Pay "luck" (Party n) (AvailableMoney "luck" token) token Close
    payingOfFeesDone ns  =
        When
            (map choice [(i, n) | (i, n) <- zip [1..] playerNames, n `elem` ns])
            (Slot 15)
            (refund ns)
      where
        choice :: (Integer, String) -> Case
        choice (i, n) =
            Case
                (Choice (ChoiceId "winner" "luck") [Bound i i])
                (Pay "luck" (Party n) (AvailableMoney "luck" token) token Close)

    refund :: [String] -> Contract
    refund []       = Close
    refund (n : ns) = Pay "luck" (Party n) (Constant feeAmount) token $ refund ns

-- Write a Marlowe contract that allows Alice and Bob to swap a token:
-- Alice has one unit of token A, Bob has one unit of token B,
-- and they want to savely swap those tokens without the risk of being cheated.

swap :: Contract
swap =
    When
        [ Case (Deposit "pot" alice (Constant 1) tokenA) $
            When
                [ Case (Deposit "pot" bob (Constant 1) tokenB) $
                    bothHaveDeposited
                ]
                (Slot 15)
                (Pay "pot" (Party alice) (Constant 1) tokenA Close)
        , Case (Deposit "pot" bob   (Constant 1) tokenB) $
            When
                [ Case (Deposit "pot" alice (Constant 1) tokenA) $
                    bothHaveDeposited
                ]
                (Slot 15)
                (Pay "pot" (Party bob) (Constant 1) tokenB Close)
        ]
        (Slot 10)
        Close
  where
    alice = "Alice"
    bob = "Bob"
    tokenA = "A"
    tokenB = "B"

    bothHaveDeposited :: Contract
    bothHaveDeposited =
        Pay "pot" (Party alice) (Constant 1) tokenB $
        Pay "pot" (Party bob)   (Constant 1) tokenA $
        Close

-- | Implement a Dutch auction as a Marlowe contract. The item on auction is represented as a (non-fungible) token.
dutchAuction :: Token    -- ^ the token to be auctioned
             -> Role     -- ^ the seller
             -> [Role]   -- ^ the bidders
             -> Amount   -- ^ the starting bid (in ada)
             -> Amount   -- ^ decrement in each slot (in ada)
             -> Amount   -- ^ minimum amount (in ada)
             -> Contract
dutchAuction token seller bidders start dec end =
    When
        [ Case (Deposit seller seller (Constant 1) token) $
            When
                (map deposit bidders)
                timeout
                Close
        ]
        (Slot 1)
        Close

  where
    deposit :: Role -> Case
    deposit bidder =
        Case (Deposit seller bidder v ada) $
            Pay seller (Party bidder) (Constant 1) token Close
      where
        v :: Value
        v =  Constant start `SubValue` (SlotIntervalStart `MulValue` Constant dec)

    timeout :: Slot
    timeout = Slot $ 1 + floor (fromIntegral (start - end) / fromIntegral dec :: Rational)

-- slot      bid
-- 0         start
-- 1         start - dec
-- 2         start - 2 * dec
-- 3         start - 3 * dec
--
-- n         start - n * dec
--
-- start - n * dec < end
-- start < end + n * dec
-- start - end < n * dec
-- n > (start - end) / dec
--
-- start = 500,000
-- dec = 7,000
-- end = 480,000
--
-- slot      bid
--
-- 0         500,000
-- 1         493,000
-- 2         486,000
-- 3         479,000 < end -- timeout should be three in this case!
--
-- (500,000 - 480,000) / 7,000 = 20,000 / 7,000 = 2.8... -> 1 + floor 2.8 = 1 + 2 = 3
--
-- start = 500,000
-- dec = 10,000
-- end = 480,000
--
-- slot      bid
--
-- 0         500,000
-- 1         490,000
-- 2         480,000
-- 3         470,000 < end -- timeout should be three in this case!
--
-- (500,000 - 480,000) / 10,000 = 20,000 / 10,000 = 2 -> 1 + floor 2 = 1 + 2 = 3

-- | An auction where each bidder can bid at most once. The highest bid wins.
oneRoundAuction :: Token  -- ^ the token to sell
                -> Role   -- ^ the seller
                -> [Role] -- ^ the bidders
                -> Amount -- ^ minimum bid in ada, assumed to be greater than zero
                -> Amount -- ^ maximum bid in ada
                -> Slot   -- ^ timeout for the seller to deposit the token
                -> Slot   -- ^ timeout for the bidders to deposit maximum bid
                -> Slot   -- ^ timeout for the bidders to make their bid
                -> Contract
oneRoundAuction token seller bidders minBid maxBid timeout1 timeout2 timeout3 =
    When
        [ Case (Deposit seller seller (Constant 1) token) $ round2 bidders [] ]
        timeout1
        Close
  where
    round2 :: [Role]   -- bidders that have not yet put down the maximum bid
           -> [Role]   -- bidders that have alread put down the maximum bid
           -> Contract
    round2 [] cs = round3 cs cs
    round2 bs cs =
        When
            (map deposit bs)
            timeout2
            (round3 cs cs)
      where
        deposit :: Role -> Case
        deposit bidder =
            Case (Deposit bidder bidder (Constant maxBid) ada) $
                round2 (filter (/= bidder) bs) (bidder : cs)

    round3 :: [Role]     -- bidders that have put down the maximum bid
           -> [Role]     -- bidders that can still bid
           -> Contract
    round3 [] _  = Close
    round3 cs [] = closeAuction cs
    round3 cs ds =
        When
            (map bid ds)
            timeout3
            (closeAuction cs)
      where
        bid :: Role -> Case
        bid bidder =
            Case (Choice (cid bidder) [Bound minBid maxBid]) $
                round3 cs (filter (/= bidder) ds)

    closeAuction :: [Role]   -- bidders that put down the deposit
                 -> Contract
    closeAuction []       = Close
    closeAuction (c : cs) =
        If
            (ChoiceValue (cid c) `ValueGT` highestBid cs)
            ( Pay seller (Party c)      (Constant 1)          token $
              Pay c      (Party seller) (ChoiceValue $ cid c) ada   $
              Close)
            (closeAuction cs)

    cid :: Role -> ChoiceId
    cid bidder = ChoiceId "bid" bidder

    highestBid :: [Role]     -- bidders that put down the deposit
               -> Value
    highestBid []       = Constant 0
    highestBid (c : cs) =
        Cond
            (ChoiceValue (cid c) `ValueGT` highestBid cs)
            (ChoiceValue (cid c))
            (highestBid cs)
