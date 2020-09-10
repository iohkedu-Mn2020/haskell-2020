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
        [ Case (Deposit "bob" "alice" (Constant 100)) Close
        ]
        (Slot 10)
        Close

-- Write a Marlowe contract where Alice pays an arbitrary amount between 100 and 200 ada to Bob.
aliceToBobChoice :: Contract
aliceToBobChoice =
    When
        [ Case (Choice (ChoiceId "amountToSend" "alice") [Bound 100 200]) $
            When
                [ Case (Deposit "bob" "alice" (ChoiceValue (ChoiceId "amountToSend" "alice"))) Close
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
        [ Case (Deposit luck "alice" fee) $
            When
                [ Case (Deposit luck "bob" fee) bothHavePaidTheirFee
                ]
                (Slot 10)
                (Pay luck (Party "alice") fee Close)
        , Case (Deposit luck "bob" fee) $
            When
                [ Case (Deposit luck "alice" fee) bothHavePaidTheirFee
                ]
                (Slot 10)
                (Pay luck (Party "bob") fee Close)
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
                (Pay luck (Party "alice") (AvailableMoney luck) Close)
            , Case (Choice (ChoiceId "aliceOrBob" "luck") [Bound 1 1])
                (Pay luck (Party "bob") (AvailableMoney luck) Close)
            ]
            (Slot 15)
            ( Pay luck (Party "alice") fee $
              Pay luck (Party "bob") fee   $
              Close)

lottery :: Amount -> [String] -> Contract
lottery _         []          = Close
lottery _         [_]         = Close
lottery feeAmount playerNames = go [] playerNames
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
            Case (Deposit "luck" n (Constant feeAmount)) $
            go (n : paid) [n' | n' <- notPaid, n' /= n]

    payingOfFeesDone :: [String] -> Contract -- gets the names of those that paid their fees
    payingOfFeesDone []  = Close
    payingOfFeesDone [n] = Pay "luck" (Party n) (AvailableMoney "luck") Close
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
                (Pay "luck" (Party n) (AvailableMoney "luck") Close)

    refund :: [String] -> Contract
    refund []       = Close
    refund (n : ns) = Pay "luck" (Party n) (Constant feeAmount) $ refund ns
