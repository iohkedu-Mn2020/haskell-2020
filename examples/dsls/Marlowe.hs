{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Marlowe where

import Data.Map
import Data.Ratio
import Data.String
import Prelude hiding (lookup)

-- Planned steps:
--
-- 1. Add "Let" to the language (in order to "save" values). DONE
-- 2. Generalise "WhenDeposit" to "When". DONE
-- 3. Add choices. DONE
-- 4. Add observables. DONE
-- 5. Add assertions. DONE
-- 6. Add timeouts.

data Contract =
    Close
  | Pay AccountId Payee Value Contract
  | If Observation Contract Contract
  | When [Case] Timeout Contract
  | Let ValueId Value Contract
  | Assert Observation Contract
  deriving Show

data Case =
    Case Action Contract
  deriving Show

data Action =
    Deposit AccountId Role Value
  | Choice ChoiceId [Bound]
  deriving Show

data Bound = Bound Integer Integer -- lower and upper bound, inclusive
  deriving Show

when :: [Case] -> Contract
when cases =
  When cases (Slot 1000) Close

whenDeposit :: AccountId -> Role -> Value -> Contract -> Contract
whenDeposit accountId role value contract =
  when [ Case (Deposit accountId role value) contract ]

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
  | UseValue ValueId
  | ChoiceValue ChoiceId
  | SlotIntervalStart
  | SlotIntervalEnd
  | Cond Observation Value Value
  deriving Show

data Observation =
    AndObs Observation Observation
  | OrObs Observation Observation
  | NotObs Observation
  | ChoseSomething ChoiceId
  | ValueGE Value Value
  | ValueGT Value Value
  | ValueLT Value Value
  | ValueLE Value Value
  | ValueEQ Value Value
  | TrueObs
  | FalseObs
  deriving Show

simpleChoice :: ChoiceId -> Integer -> Contract -> Case
simpleChoice choiceId number cont =
  Case (Choice choiceId [ Bound number number ]) cont

type Amount = Integer -- whenever we use Amount, it is supposed to be non-negative
type AccountId = Role
type Role = String
type ChoiceName = String
type ChosenNum = Integer
type Timeout = Slot

newtype ValueId = ValueId String
  deriving (Show, Eq, Ord, IsString)

data ChoiceId = ChoiceId ChoiceName Role
  deriving (Show, Eq, Ord)

newtype Slot = Slot { getSlot :: Integer }
  deriving (Show, Eq, Ord)

-- The ivTo slot should always be greater than or equal to the ivFrom slot.
data SlotInterval = SlotInterval { ivFrom :: Slot, ivTo :: Slot }
  deriving Show

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

letBasedContract :: Contract
letBasedContract =
  Pay "alice" (Account "carol") (Constant 60) $
  Let "oneThird" (Scale (1/3) (AvailableMoney "carol")) $
  Pay "carol" (Account "bob") (UseValue "oneThird") $
  Pay "carol" (Account "alice") (UseValue "oneThird") $
  Close

payoutContract :: Contract
payoutContract =
  Pay "alice" (Party "carol") (Constant 60) $
  Close

depositContract :: Contract
depositContract =
  Pay "alice" (Account "carol") (Constant 20) $
  whenDeposit "bob" "bob" (Constant 100) $
  Close

combinedPayment :: Contract
combinedPayment =
  whenDeposit "lars" "lars" (Constant 100) $
  whenDeposit "andres" "andres" (Constant 100) $
  Pay "lars" (Party "alejandro") (Constant 100) $
  Pay "andres" (Party "alejandro") (Constant 100) $
  Close

combinedPayment' :: Contract
combinedPayment' =
  whenDeposit "alejandro" "lars" (Constant 100) $
  whenDeposit "alejandro" "andres" (Constant 100) $
  Close

combinedPaymentAnyOrder :: Contract
combinedPaymentAnyOrder =
  when
    [ Case (Deposit "lars" "lars" (Constant 100)) $
        when [ Case (Deposit "andres" "andres" (Constant 100)) payout ]
    , Case (Deposit "andres" "andres" (Constant 100)) $
        when [ Case (Deposit "lars" "lars" (Constant 100)) payout ]
    ]
  where
    payout :: Contract
    payout =
      Pay "lars" (Party "alejandro") (Constant 100) $
      Pay "andres" (Party "alejandro") (Constant 100) $
      Close

split1 :: Contract
split1 =
  whenDeposit "carol" "alice" (Constant 100) $
  whenDeposit "carol" "bob" (Constant 100) $
  when
    [ simpleChoice (ChoiceId "aliceOrBob" "carol") alice (payout "alice")
    , simpleChoice (ChoiceId "aliceOrBob" "carol") bob (payout "bob")
    ]
  where
    alice :: ChosenNum
    alice = 0

    bob :: ChosenNum
    bob = 1

    payout :: Role -> Contract
    payout role =
      Pay "carol" (Party role) (Constant 200) $
      Close

split1' :: Contract
split1' =
  whenDeposit "carol" "alice" (Constant 100) $
  whenDeposit "carol" "bob" (Constant 100) $
  when
    [ Case (Choice aliceShare [Bound 0 0, Bound 200 200]) $
        Pay "carol" (Party "alice") (ChoiceValue aliceShare) $
        Pay "carol" (Party "bob") (SubValue (Constant 200) (ChoiceValue aliceShare)) $
        Close
    ]
  where
    aliceShare :: ChoiceId
    aliceShare = ChoiceId "aliceShare" "carol"

split2 :: Contract
split2 =
  whenDeposit "carol" "alice" (Constant 100) $
  whenDeposit "carol" "bob" (Constant 100) $
  when
    [ Case (Choice aliceShare [Bound 0 200]) $
        Pay "carol" (Party "alice") (ChoiceValue aliceShare) $
        Pay "carol" (Party "bob") (SubValue (Constant 200) (ChoiceValue aliceShare)) $
        Close
    ]
  where
    aliceShare :: ChoiceId
    aliceShare = ChoiceId "aliceShare" "carol"

split3 :: Contract
split3 =
  whenDeposit "carol" "alice" (Constant 100) $
  whenDeposit "carol" "bob" (Constant 100) $
  when
    [ Case (Choice aliceShare [Bound 0 200]) $
        Pay "carol" (Party "alice") (ChoiceValue aliceShare) $
        If (ValueGE (AvailableMoney "carol") (Constant 100))
          ( (Pay "carol" (Party "bob") (Constant 100)) $
            (Pay "carol" (Party "eve") (AvailableMoney "carol")) $
            Close
          )
          (Pay "carol" (Party "bob") (SubValue (Constant 200) (ChoiceValue aliceShare)) $ Close)
    ]
  where
    aliceShare :: ChoiceId
    aliceShare = ChoiceId "aliceShare" "carol"

inBounds :: ChosenNum -> [Bound] -> Bool
inBounds number =
  any (\ (Bound lower upper) -> lower <= number && number <= upper)

data ReduceWarning =
    ReduceNoWarning
  | ReduceNonPositivePay AccountId Payee Integer -- the integer is supposed to be non-positive
  | ReducePartialPay AccountId Payee Amount Amount -- first is money actually paid, second is money expected to be paid
  | ReduceShadowing ValueId Integer Integer -- shadowed value, new value
  | ReduceAssertionFailed
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
  | AmbiguousSlotIntervalReductionError
  deriving Show

data Environment =
  Environment
    { slotInterval :: SlotInterval
    }
  deriving Show

data State =
  State
    { accounts    :: Map AccountId Amount -- everything stored here should have a positive amount
    , boundValues :: Map ValueId Integer
    , choices     :: Map ChoiceId ChosenNum
    }
  deriving Show

exampleState :: State
exampleState =
  State
    { accounts    = fromList [] -- [("alice", 60), ("eve", 1_000_000)]
    , boundValues = empty
    , choices     = empty
    }

evalValue :: Environment -> State -> Value -> Integer
evalValue environment state value = go value
  where
    go :: Value -> Integer
    go (AvailableMoney accountId)   = moneyInAccount accountId (accounts state)
    go (Constant integer)           = integer
    go (NegValue val)               = negate (go val)
    go (AddValue val1 val2)         = go val1 + go val2
    go (SubValue val1 val2)         = go val1 - go val2
    go (MulValue val1 val2)         = go val1 * go val2
    go (Scale factor val)           = evalScale factor (go val)
    go (UseValue valueId)           = findWithDefault 0 valueId (boundValues state)
    go (ChoiceValue choiceId)       = findWithDefault 0 choiceId (choices state)
    go SlotIntervalStart            = getSlot (ivFrom (slotInterval environment))
    go SlotIntervalEnd              = getSlot (ivTo (slotInterval environment))
    go (Cond observation val1 val2) = if evalObservation environment state observation then go val1 else go val2

evalScale :: Rational -> Integer -> Integer
evalScale factor integer =
      let
        n      = numerator factor
        d      = denominator factor
        nn     = integer * n
        (q, r) = quotRem nn d
      in
        if abs r * 2 < abs d then q else q + signum nn * signum d

evalObservation :: Environment -> State -> Observation -> Bool
evalObservation environment state observation = go observation
  where
    goValue :: Value -> Integer
    goValue = evalValue environment state

    go :: Observation -> Bool
    go (AndObs observation1 observation2) = go observation1 && go observation2
    go (OrObs observation1 observation2)  = go observation1 || go observation2
    go (NotObs observation1)              = not (go observation1)
    go (ChoseSomething choiceId)          = member choiceId (choices state)
    go (ValueGE value1 value2)            = goValue value1 >= goValue value2
    go (ValueGT value1 value2)            = goValue value1 >  goValue value2
    go (ValueLT value1 value2)            = goValue value1 <  goValue value2
    go (ValueLE value1 value2)            = goValue value1 <= goValue value2
    go (ValueEQ value1 value2)            = goValue value1 == goValue value2
    go TrueObs                            = True
    go FalseObs                           = False

refundOne :: Map AccountId Amount -> Maybe (Payment, Map AccountId Amount)
refundOne accts = do
  ((accountId, amount), newAccts) <- minViewWithKey accts
  return (Payment accountId amount, newAccts)

reduceContractStep :: Environment -> State -> Contract -> ReduceStepResult
reduceContractStep environment state contract = go contract
  where
    go :: Contract -> ReduceStepResult
    go Close =
      case refundOne (accounts state) of
        Nothing -> NotReduced
        Just (payment, newAccounts) ->
          Reduced ReduceNoWarning (ReduceWithPayment payment) (state { accounts = newAccounts }) Close

    go (Pay from to value cont) =
      let
        expectedToBePaid :: Integer
        expectedToBePaid = evalValue environment state value
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

    go (If observation cont1 cont2) =
      let
        cont :: Contract
        cont = if evalObservation environment state observation then cont1 else cont2
      in
        Reduced ReduceNoWarning ReduceNoPayment state cont

    go (When _cases timeout cont)
      | ivTo (slotInterval environment) < timeout    = NotReduced -- timeout still in "future"
      | timeout <= ivFrom (slotInterval environment) = Reduced ReduceNoWarning ReduceNoPayment state cont -- timeout occurred
      | otherwise                                    = AmbiguousSlotIntervalReductionError

    go (Let valueId value cont) =
      let
        evaluatedValue :: Integer
        evaluatedValue = evalValue environment state value

        newBoundValues :: Map ValueId Integer
        newBoundValues = insert valueId evaluatedValue (boundValues state)

        newState :: State
        newState = state { boundValues = newBoundValues }

        warning :: ReduceWarning
        warning =
          case lookup valueId (boundValues state) of
            Nothing      -> ReduceNoWarning
            Just integer -> ReduceShadowing valueId integer evaluatedValue
      in
        Reduced warning ReduceNoPayment newState cont

    go (Assert observation cont) =
      let
        evaluatedObservation :: Bool
        evaluatedObservation = evalObservation environment state observation

        warning :: ReduceWarning
        warning
          | evaluatedObservation = ReduceNoWarning
          | otherwise            = ReduceAssertionFailed
      in
        Reduced warning ReduceNoPayment state cont

data ReduceResult =
    ContractQuiescent [ReduceWarning] [Payment] State Contract
  | RRAmbiguousSlotIntervalError
  deriving Show

reduceContractUntilQuiescent :: Environment -> State -> Contract -> ReduceResult
reduceContractUntilQuiescent environment state0 contract0 =
  let
    reductionLoop :: State -> Contract -> [ReduceWarning] -> [Payment] -> ReduceResult
    reductionLoop state contract warnings payments =
      case reduceContractStep environment state contract of
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
        AmbiguousSlotIntervalReductionError ->
          RRAmbiguousSlotIntervalError
  in
    reductionLoop state0 contract0 [] []

data Input =
    IDeposit AccountId Role Amount
  | IChoice ChoiceId ChosenNum

data ApplyWarning =
    ApplyNoWarning
  | ApplyNonPositiveDeposit Role AccountId Integer
  deriving Show

data ApplyResult =
    Applied ApplyWarning State Contract -- TODO: warnings
  | ApplyNoMatchError
  deriving Show

-- We are assuming that in the current context, the contract is quiescent.
-- That in particular means that the contract should not be waiting on a
-- When at a point in time when the timeout has already occurred.
--
applyInput :: Environment -> State -> Input -> Contract -> ApplyResult
applyInput environment state input (When cases _timeout _cont) = applyCases environment state input cases
applyInput _           _     _     _                           = ApplyNoMatchError

applyCases :: Environment -> State -> Input -> [Case] -> ApplyResult
applyCases environment state input cases =
  case (input, cases) of
    (_, []) ->
      ApplyNoMatchError
    (IDeposit accountId1 role1 amount1, Case (Deposit accountId2 role2 value2) cont : rest) ->
      let
        amount2 :: Integer -- we do not know yet whether this is negative or positive
        amount2 = evalValue environment state value2

        warning :: ApplyWarning
        warning
          | amount2 > 0 = ApplyNoWarning
          | otherwise   = ApplyNonPositiveDeposit role2 accountId2 amount2

        newState :: State
        newState = state { accounts = addMoneyToAccount accountId1 amount1 (accounts state) }
      in
        if accountId1 == accountId2 && role1 == role2 && amount1 == amount2
          then Applied warning newState cont
          else applyCases environment state input rest
    (IChoice choiceId1 chosenNum1, Case (Choice choiceId2 bounds2) cont : rest) ->
      let
        newState :: State
        newState = state { choices = insert choiceId1 chosenNum1 (choices state) }
      in
        if choiceId1 == choiceId2 && inBounds chosenNum1 bounds2
          then Applied ApplyNoWarning newState cont
          else applyCases environment state input rest
    (_, _ : rest) ->
      applyCases environment state input rest

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
