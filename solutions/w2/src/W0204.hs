{-# LANGUAGE RecordWildCards #-}

-- | W2.4 Transactions
module W0204
    ( -- * Types
      Output (..), Address, Input (..), Id, Index, Transaction (..), UTxOs
      -- * Subtask W2.4.1
    , processTransaction, processTransactions
      -- * Subtask W2.4.2
    , genesis, tx1, tx2, tx3, tx4, tx5
      -- * Subtask W2.4.3
    , ErrorState (..), throwError, get, put
      -- * Subtask W2.4.4
    , processTransaction', processTransactions'
    ) where

import           Control.Monad
import           Data.List     (foldl')
import           Data.Map      (Map)
import qualified Data.Map      as M

data Output = Output
    { oValue   :: Int
    , oAddress :: Address
    } deriving (Show, Read, Eq, Ord)

type Address = String

data Input = Input
    { iPrevious :: Id
    , iIndex    :: Index
    } deriving (Show, Read, Eq, Ord)

type Id = Int
type Index = Int

data Transaction = Transaction
    { tId      :: Id
    , tInputs  :: [Input]
    , tOutputs :: [Output]
    } deriving (Show, Read, Eq, Ord)

type UTxOs = Map Input Output

-- Subtask W2.4.1

-- | Processes one transaction:
--
--   1. Processes all inputs, deletes them from the @UTxOs@ and calculates the
--   sum of their values.
--
--   2. Processes all outputs, inserts them into the @UTxOs@ and calculates
--   the sum of their values.
--
--   3. Checks whether the sum of input values is at least as large as the sum
--   of output values.
--
-- If all three steps succeed, the updated @UTxOs@ are returned as a @`Right`@;
-- otherwise an error is reported as a @`Left`@.
processTransaction :: Transaction -> UTxOs -> Either String UTxOs
processTransaction Transaction{..} utxos = do
    (inputValue, utxos') <- processInputs tId tInputs utxos
    let (outputValue, utxos'') = processOutputs tId tOutputs utxos'
    when (inputValue < outputValue) $
        Left $ "input value " ++ show inputValue ++
               " less than output value " ++ show outputValue ++
               " in transaction " ++ show tId
    return utxos''
--
-- | Processes one input. Returns a @`Left`@ if the input does not exist;
-- deletes it from the @UTxOs@ and returns its value as a @`Right`@ if it does.
processInput :: Id -> Input -> UTxOs -> Either String (Int, UTxOs)
processInput txId input utxos = case M.lookup input utxos of
    Nothing         -> Left $ "non-existent input " ++ show input ++ " in transaction " ++ show txId
    Just Output{..} -> return (oValue, M.delete input utxos)

-- | Processes a list of inputs.
-- Returns a @`Left`@ if any of the inputs does not exist;
-- deletes all inputs from the @UTxOs@ and returns the
-- sum of their values as a @`Right`@ otherwise.
--
-- Note that this automatically checks for duplicate inputs,
-- because if there indeed is such a duplicate,
-- then it will have been removed from the @UTxOs@ the first
-- time it was encountered, thus failing the second time.
processInputs :: Id -> [Input] -> UTxOs -> Either String (Int, UTxOs)
processInputs txId inputs utxos = foldM f (0, utxos) inputs
  where
    f :: (Int, UTxOs) -> Input -> Either String (Int, UTxOs)
    f (acc, utxos') input = do
        (value, utxos'') <- processInput txId input utxos'
        return (acc + value, utxos'')

-- |Processes one output by inserting it into the @UTxOs@.
-- Returns the updated @UTxOs@.
processOutput :: Id -> Index -> Output -> UTxOs -> UTxOs
processOutput txId i output utxos =
    let input = Input {iPrevious = txId, iIndex = i}
    in  M.insert input output utxos

-- |Processes a list of outputs by inserting them into the @UTxOs@.
-- Returns the sum of their values and the updated @UTxOs@.
processOutputs :: Id -> [Output] -> UTxOs -> (Int, UTxOs)
processOutputs txId outputs utxos = foldl' f (0, utxos) $ zip [0..] outputs
  where
    f :: (Int, UTxOs) -> (Int, Output) -> (Int, UTxOs)
    f (acc, utxos') (i, output) = (acc + oValue output, processOutput txId i output utxos')

-- |Processes a list of transactions by simply invoking @`processTransaction`@
-- for each transaction in turn, failing if this fails for one transaction,
-- otherwise passing the updated @UTxOs@ to the next transaction.
-- If all transactions can be processed, the final @UTxOs@ are returned.
processTransactions :: [Transaction] -> UTxOs -> Either String UTxOs
processTransactions txs utxos = foldM (flip processTransaction) utxos txs

-- Subtask W2.4.2

-- | The initial @'UTxOs'@.
genesis :: UTxOs
genesis = M.fromList
    [ (Input 0 0, Output 1000 "Alejandro")
    , (Input 0 1, Output 1000 "Andres")
    , (Input 0 2, Output 1000 "Lars")
    ]

-- | A transaction which is invalid, because one of its inputs
-- does not exist.
--
-- >>> processTransaction tx1 genesis
-- Left ...
--
tx1 :: Transaction
tx1 = Transaction 1 [Input 0 3] []

-- | A transaction which is invalid, because the sum of input values
-- is smaller than the sum of output values.
--
-- >>> processTransaction tx2 genesis
-- Left ...
--
tx2 :: Transaction
tx2 = Transaction 1 [Input 0 0] [Output 2000 "Charles"]

-- | A valid transaction.
--
-- >>> processTransaction tx3 genesis
-- Right ...
--
tx3 :: Transaction
tx3 = Transaction 1 [Input 0 0] [Output 100 "Charles", Output 900 "Alejandro"]

-- | Another valid transaction, but one that has an input
-- in common with @'tx3'@.
--
-- >>> processTransaction tx4 genesis
-- Right ...
--
-- >>> processTransactions [tx3, tx4] genesis
-- Left ...
--
tx4 :: Transaction
tx4 = Transaction 1 [Input 0 0, Input 0 1] [Output 100 "Charles", Output 950 "Alejandro", Output 950 "Andres"]

-- | A valid transaction, which can be combined with @'tx4'@.
--
-- >>> processTransaction tx5 genesis
-- Right ...
--
-- >>> processTransactions [tx4, tx5] genesis
-- Right ...
--
tx5 :: Transaction
tx5 = Transaction 2 [Input 0 2] [Output 50 "Charles", Output 950 "Lars"]

-- Subtask W2.4.3

newtype ErrorState s a = ErrorState {runErrorState :: s -> Either String (a, s)}

instance Functor (ErrorState s) where
    fmap = liftM

instance Applicative (ErrorState s) where
    pure = return
    (<*>) = ap

instance Monad (ErrorState s) where

    return a = ErrorState $ \s -> Right (a, s)

    m >>= cont = ErrorState $ \s -> case runErrorState m s of
        (Left e)        -> Left e
        (Right (a, s')) -> runErrorState (cont a) s'

throwError :: String -> ErrorState s a
throwError e = ErrorState $ \_ -> Left e

get :: ErrorState s s
get = ErrorState $ \s -> Right (s, s)

put :: s -> ErrorState s ()
put s = ErrorState $ \_ -> Right ((), s)

-- |Convenience function for the common case of @`get`@ting the state,
-- applying a function to it and @`put`@ting the new state.
modify :: (s -> s) -> ErrorState s ()
modify f = get >>= \s -> put (f s)

-- Subtask W2.4.4

-- |@`ErrorState`@-version of @`processTransaction`@.
processTransactionES :: Transaction -> ErrorState UTxOs ()
processTransactionES Transaction{..} = do
    inputValue  <- processInputsES tId tInputs
    outputValue <- processOutputsES tId tOutputs
    when (inputValue < outputValue) $
        throwError $ "input value " ++ show inputValue ++
                     " less than output value " ++ show outputValue ++
                     " in transaction " ++ show tId

-- |@`ErrorState`@-version of @`processTransactions`@.
processTransactionsES :: [Transaction] -> ErrorState UTxOs ()
processTransactionsES = mapM_ processTransactionES

-- |@`ErrorState`@-version of @`processInput`@.
processInputES :: Id -> Input -> ErrorState UTxOs Int
processInputES txId input = do
    utxos <- get
    case M.lookup input utxos of
        Nothing         -> throwError $ "non-existent input " ++ show input ++ " in transaction " ++ show txId
        Just Output{..} -> do
            put $ M.delete input utxos
            return oValue

-- |@`ErrorState`@-version of @`processInputs`@.
processInputsES :: Id -> [Input] -> ErrorState UTxOs Int
processInputsES txId inputs = sum <$> mapM (processInputES txId) inputs

-- |@`ErrorState`@-version of @`processOutput`@.
-- Returns the output value.
processOutputES :: Id -> Index -> Output -> ErrorState UTxOs Int
processOutputES txId i output = do
    modify $ M.insert (Input txId i) output
    return $ oValue output

-- |@`ErrorState`@-version of @`processOutputs`@.
processOutputsES :: Id -> [Output] -> ErrorState UTxOs Int
processOutputsES txId outputs = sum <$> zipWithM (processOutputES txId) [0..] outputs

-- |
-- >>> processTransaction' tx1 genesis
-- Left ...
--
-- >>> processTransaction' tx2 genesis
-- Left ...
--
-- >>> processTransaction' tx3 genesis
-- Right ...
--
-- >>> processTransaction' tx4 genesis
-- Right ...
--
-- >>> processTransaction' tx5 genesis
-- Right ...
--
processTransaction' :: Transaction -> UTxOs -> Either String UTxOs
processTransaction' tx = processTransactions' [tx]

-- |
-- >>> processTransactions' [tx3, tx4] genesis
-- Left ...
--
-- >>> processTransactions' [tx4, tx5] genesis
-- Right ...
--
processTransactions' :: [Transaction] -> UTxOs -> Either String UTxOs
processTransactions' txs utxos = snd <$> runErrorState (processTransactionsES txs) utxos
