{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wall #-}
module Marlowe1 where

import Data.Map
import Data.Ratio

-- Next steps:
--
-- 1. Generate warnings as part of the reduction semantics. DONE
-- 2. Add a function that reduces a contract as far as possible. DONE
-- 3. Generalise from amounts to values (in Pay). DONE
-- 4. Generalise from to-accountid to payee (in Pay). [to get money out] DONE
-- 5. Change the semantics of Close to refund all account owners. DONE
-- 6. Add a construct to the Contract language that waits for a deposit (plus semantics). DONE

data Contract =
    Close
  | Pay AccountId Payee Value Contract
  | WhenDeposit AccountId Role Value Contract
  deriving Show

data Payee =
    Account AccountId
  | Party Role
  deriving (Show, Eq)

data Value =
    AvailableMoney AccountId
  | Constant Integer
  | NegValue Value
  | AddValue Value Value
  | SubValue Value Value
  | MulValue Value Value
  | Scale Rational Value
  deriving Show

type Amount = Integer -- whenever we use Amount, it is supposed to be non-negative
type AccountId = Role
type Role = String


exampleContract :: Contract
exampleContract =
  Pay "alice" (Account "carol") (Constant 60) $
  Pay "carol" (Account "bob"  ) (Constant 20) $
  Pay "carol" (Account "alice") (Constant 20) $
  Close

anotherContract :: Contract
anotherContract =
  Pay "alice" (Account "carol") (Constant 60) $
  Pay "carol" (Account "bob"  ) (Scale (1/3) (AvailableMoney "carol")) $
  Pay "carol" (Account "alice") (AvailableMoney "bob") $
  Close

payoutContract :: Contract
payoutContract =
  Pay "alice" (Party "carol") (Constant 60) $
  Close

depositContract :: Contract
depositContract =
  Pay "alice" (Account "carol") (Constant 20) $
  WhenDeposit "bob" "bob" (Constant 100) $
  Close

combinedPayment :: Contract
combinedPayment =
  WhenDeposit "lars" "lars" (Constant 100) $
  WhenDeposit "andres" "andres" (Constant 100) $
  Pay "lars" (Party "alejandro") (Constant 100) $
  Pay "andres" (Party "alejandro") (Constant 100) $
  Close

combinedPayment' :: Contract
combinedPayment' =
  WhenDeposit "alejandro" "lars" (Constant 100) $
  WhenDeposit "alejandro" "andres" (Constant 100) $
  Close

data ReduceWarning =
    ReduceNoWarning
  | ReduceNonPositivePay AccountId Payee Integer -- the integer is supposed to be non-positive
  | ReducePartialPay AccountId Payee Amount Amount -- first is money actually paid, second is money expected to be paid
  deriving (Show, Eq)

data ReduceEffect =
    ReduceNoPayment
  | ReduceWithPayment Payment
  deriving Show

data Payment = Payment Role Amount
  deriving Show

data ReduceStepResult =
    Reduced ReduceWarning ReduceEffect State Contract
  | NotReduced
  deriving Show

data State =
  State
    { accounts :: Map AccountId Amount -- everything stored here should have a positive amount
    }
  deriving Show

exampleState :: State
exampleState =
  State
    { accounts = fromList [("alice", 30), ("eve", 1_000_000)] }

evalValue :: State -> Value -> Integer
evalValue state value = go value
  where
    go :: Value -> Integer
    go (AvailableMoney accountId) = moneyInAccount accountId (accounts state)
    go (Constant integer)         = integer
    go (NegValue val)             = negate (go val)
    go (AddValue val1 val2)       = go val1 + go val2
    go (SubValue val1 val2)       = go val1 - go val2
    go (MulValue val1 val2)       = go val1 * go val2
    go (Scale factor val)         = evalScale factor (go val)

evalScale :: Rational -> Integer -> Integer
evalScale factor integer =
      let
        n      = numerator factor
        d      = denominator factor
        nn     = integer * n
        (q, r) = quotRem nn d
      in
        if abs r * 2 < abs d then q else q + signum nn * signum d

refundOne :: Map AccountId Amount -> Maybe (Payment, Map AccountId Amount)
refundOne accts = do
  ((accountId, amount), newAccts) <- minViewWithKey accts
  return (Payment accountId amount, newAccts)

reduceContractStep :: State -> Contract -> ReduceStepResult
reduceContractStep state contract = go contract
  where
    go :: Contract -> ReduceStepResult
    go Close =
      case refundOne (accounts state) of
        Nothing -> NotReduced
        Just (payment, newAccounts) ->
          Reduced ReduceNoWarning (ReduceWithPayment payment) (state { accounts = newAccounts }) Close

    go (Pay from to value cont) =
      let
        expectedToBePaid = evalValue state value
      in
        if expectedToBePaid <= 0
          then Reduced (ReduceNonPositivePay from to expectedToBePaid) ReduceNoPayment state cont
          else
            let
              balance :: Amount
              balance = moneyInAccount from (accounts state)

              paid :: Amount
              paid = min balance expectedToBePaid

              newBalance :: Amount
              newBalance = balance - paid

              warning :: ReduceWarning
              warning
                | paid < expectedToBePaid = ReducePartialPay from to paid expectedToBePaid
                | otherwise               = ReduceNoWarning

              newAccounts :: Map AccountId Amount
              newAccounts = updateMoneyInAccount from newBalance (accounts state)

              effect :: ReduceEffect
              finalAccounts :: Map AccountId Amount
              (effect, finalAccounts) = giveMoney to paid newAccounts
            in
              Reduced warning effect (state { accounts = finalAccounts }) cont

    go (WhenDeposit _accountId _role _value _cont) =
      NotReduced

data ReduceResult =
  ContractQuiescent [ReduceWarning] [Payment] State Contract
  deriving Show

reduceContractUntilQuiescent :: State -> Contract -> ReduceResult
reduceContractUntilQuiescent state0 contract0 =
  let
    reductionLoop :: State -> Contract -> [ReduceWarning] -> [Payment] -> ReduceResult
    reductionLoop state contract warnings payments =
      case reduceContractStep state contract of
        Reduced warning effect newState cont ->
          let
            newWarnings :: [ReduceWarning]
            newWarnings
              | warning == ReduceNoWarning = warnings
              | otherwise                  = warning : warnings

            newPayments :: [Payment]
            newPayments =
              case effect of
                ReduceNoPayment           -> payments
                ReduceWithPayment payment -> payment : payments
          in
            reductionLoop newState cont newWarnings newPayments
        NotReduced ->
          ContractQuiescent (reverse warnings) (reverse payments) state contract
  in
    reductionLoop state0 contract0 [] []

data Input = IDeposit AccountId Role Amount

data ApplyResult =
    Applied State Contract -- TODO: warnings
  | ApplyNoMatchError
  deriving Show

applyInput :: State -> Input -> Contract -> ApplyResult
applyInput state (IDeposit accountId1 role1 amount1) (WhenDeposit accountId2 role2 value2 cont) =
  let
    amount2 :: Amount
    amount2 = evalValue state value2

    newState :: State
    newState = state { accounts = addMoneyToAccount accountId1 amount1 (accounts state) }
  in
    if accountId1 == accountId2 && role1 == role2 && amount1 == amount2
      then Applied newState cont
      else ApplyNoMatchError
applyInput _state _input _contract =
  ApplyNoMatchError

-- look up current money in account
-- assume 0 if account "does not exist"
moneyInAccount :: AccountId -> Map AccountId Amount -> Amount
moneyInAccount = findWithDefault 0

-- set account to given amount,
-- if amount is 0 delete from the mapping
updateMoneyInAccount :: AccountId -> Amount -> Map AccountId Amount -> Map AccountId Amount
updateMoneyInAccount accountId amount
  | amount > 0 = insert accountId amount
  | otherwise  = delete accountId

-- can be defined in terms of updateMoneyInAccount,
-- add the amount to the current value stored in the mapping
giveMoney :: Payee -> Amount -> Map AccountId Amount -> (ReduceEffect, Map AccountId Amount)
giveMoney (Account accountId) amount accts = (ReduceNoPayment, addMoneyToAccount accountId amount accts)
giveMoney (Party role)        amount accts = (ReduceWithPayment (Payment role amount), accts)

addMoneyToAccount :: AccountId -> Amount -> Map AccountId Amount -> Map AccountId Amount
addMoneyToAccount accountId amount accs =
  let
    balance    = moneyInAccount accountId accs
    newBalance = balance + amount
  in
    updateMoneyInAccount accountId newBalance accs
