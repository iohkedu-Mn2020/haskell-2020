module Transactions where

import Data.Maybe (fromMaybe)
import Prelude    hiding (lookup)
import Tables

-- START HERE AFTER Tables.hs

-- This is the transactions datatype from the slides,
-- using record syntax.

data Transaction =
  Transaction
    { trAmount :: Amount
    , trFrom   :: Account
    , trTo     :: Account
    }
  deriving (Eq, Show)

type Amount  = Int
type Account = String

-- Both declarations below define transactions. The
-- first uses the 'Transaction' constructor normally,
-- the second uses record syntax during construction.
--
-- In the record version, we can assign the fields
-- in a different oder; otherwise, both versions are
-- equivalent.

transaction1 :: Transaction
transaction1 = Transaction 10 "Andres" "Lars"

transaction2 :: Transaction
transaction2 =
  Transaction
    { trAmount = 7
    , trFrom   = "Lars"
    , trTo     = "Alejandro"
    }

-- Task Transactions-1.
--
-- Flip a transaction, by producing a transaction of
-- the negative amount in the opposite direction.

-- |
-- >>> flipTransaction transaction1
-- Transaction {trAmount = -10, trFrom = "Lars", trTo = "Andres"}
--
flipTransaction :: Transaction -> Transaction
flipTransaction t = Transaction
    { trAmount = - (trAmount t)
    , trFrom   = trTo t
    , trTo     = trFrom t
    }

-- Task Transactions-2.
--
-- Normalize a transaction by flipping it if and only
-- if the transaction amount is negative.

normalizeTransaction :: Transaction -> Transaction
normalizeTransaction t
    | trAmount t < 0 = flipTransaction t
    | otherwise      = t

-- Task Transactions-3.
--
-- Re-implement 'processTransaction' from the slides,
-- but use the function 'alter' that you defined in
-- the Tables tasks.

type Accounts = Table Account Amount

-- |
-- >>> let a = processTransaction transaction1 $ insert "Andres" 30 empty
-- >>> lookup "Andres" a
-- Just 20
-- >>> lookup "Lars" a
-- Just 10
-- >>> lookup "Alejandro" a
-- Nothing
-- >>> let b = processTransaction transaction2 a
-- >>> lookup "Lars" b
-- Just 3
-- >>> lookup "Alejandro" b
-- Just 7
--
processTransaction :: Transaction -> Accounts -> Accounts
processTransaction tx =
    let x = trAmount tx
    in  alter (add (- x)) (trFrom tx) .
        alter (add    x ) (trTo   tx)
  where
    add :: Amount -> Maybe Amount -> Maybe Amount
    add x Nothing  = Just x
    add x (Just y) = Just (x + y)

-- Task Transactions-4.
--
-- Verify that you can no longer pattern match on the
-- 'Tables' constructor if you have hidden the 'Tables'
-- constructor from the export list as requested in the
-- Tables tasks.

-- Task Transactions-5.
--
-- Process a list of transactions one by one.

-- |
-- >>> let a = processTransactions [transaction1, transaction2] empty
-- >>> lookup "Andres" a
-- Just (-10)
-- >>> lookup "Lars" a
-- Just 3
-- >>> lookup "Alejandro" a
-- Just 7
--
processTransactions :: [Transaction] -> Accounts -> Accounts
processTransactions []         a = a
processTransactions (tx : txs) a = processTransactions txs $ processTransaction tx a

-- Task Transactions-6.
--
-- Write a version of 'processTransaction' that fails
-- if and only if the new balances would be negative.

-- |
-- >>> let Just a = processTransaction' transaction1 $ insert "Andres" 30 empty
-- >>> lookup "Andres" a
-- Just 20
-- >>> lookup "Lars" a
-- Just 10
--
-- >>> processTransaction' transaction1 empty
-- Nothing
--
processTransaction' :: Transaction -> Accounts -> Maybe Accounts
processTransaction' tx a =
    let f    = trFrom tx
        t    = trTo   tx
        fOld = fromMaybe 0 $ lookup f a
        tOld = fromMaybe 0 $ lookup t a
        x    = trAmount tx
        fNew = fOld - x
        tNew = tOld + x
    in  if fNew < 0 || tNew < 0 then Nothing
                                else Just $ insert f fNew $ insert t tNew a

-- Task Transactions-7.
--
-- Write a versionof 'processTransactions' that fails
-- if any of the individual transactions fail.

-- |
-- >>> let Just a = processTransactions' [transaction1, transaction2] $ insert "Andres" 50 empty
-- >>> lookup "Andres" a
-- Just 40
-- >>> lookup "Lars" a
-- Just 3
-- >>> lookup "Alejandro" a
-- Just 7
--
-- >>> processTransactions' [transaction1, transaction2] empty
-- Nothing
--
processTransactions' :: [Transaction] -> Accounts -> Maybe Accounts
processTransactions' []         a = Just a
processTransactions' (tx : txs) a = case processTransaction' tx a of
    Nothing -> Nothing
    Just a' -> processTransactions' txs a'

-- Task Transactions-8.
--
-- Make your package depend on the @containers@ package.
-- For this module, switch from using the 'Tables' datatype
-- to the 'Map' datatype from the 'Data.Map' module, and
-- verify that everything still works.

-- GO BACK to Datatypes.hs
