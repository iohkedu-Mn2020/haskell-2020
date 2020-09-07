module Marlowe0 where

import Data.Map

data Contract =
    Close
  | Pay AccountId AccountId Amount Contract

type Amount = Integer -- whenever we use Amount, it is supposed to be non-negative
type AccountId = String

data ReduceStepResult =
    Reduced State Contract
  | NotReduced

data State =
  State
    { accounts :: Map AccountId Amount -- everything stored here should have a positive amount
    }

reduceContractStep :: State -> Contract -> ReduceStepResult
reduceContractStep state contract = go contract
  where
    go :: Contract -> ReduceStepResult
    go Close = NotReduced
    go (Pay from to amount cont) =
      if amount <= 0
        then Reduced state cont -- TODO: issue a warning!
        else
          let
            balance :: Amount
            balance = moneyInAccount from (accounts state)

            paid :: Amount
            paid = min balance amount

            newBalance :: Amount
            newBalance = balance - paid

            newAccounts :: Map AccountId Amount
            newAccounts = updateMoneyInAccount from newBalance (accounts state)

            finalAccounts :: Map AccountId Amount
            finalAccounts = giveMoney to paid newAccounts
          in
            Reduced (state { accounts = finalAccounts }) cont

-- look up current money in account
-- assume 0 if account "does not exist"
moneyInAccount :: AccountId -> Map AccountId Amount -> Amount
moneyInAccount = undefined

-- set account to given amount,
-- if amount is 0 delete from the mapping
updateMoneyInAccount :: AccountId -> Amount -> Map AccountId Amount -> Map AccountId Amount
updateMoneyInAccount = undefined

-- can be defined in terms of updateMoneyInAccount,
-- add the amount to the current value stored in the mapping
giveMoney :: AccountId -> Amount -> Map AccountId Amount -> Map AccountId Amount
giveMoney = undefined
