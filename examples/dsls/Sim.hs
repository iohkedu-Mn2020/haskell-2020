{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sim where

import           Control.Monad
import qualified Control.Monad.State as S
import qualified Data.Map            as M
import           Text.Printf         (printf)
import           Text.Read           (readMaybe)
import           Marlowe

-- goal: simulateIO :: Contract -> IO ()

class Monad m => MonadSim m where
    currentSlot :: m Slot
    getState :: m State
    traceWarning :: Either ReduceWarning ApplyWarning -> m ()
    tracePayment :: Payment -> m ()
    pickInput :: [Action] -> Slot -> m (Either Slot Input)
    setSlot :: Slot -> m ()
    setState :: State -> m ()
    traceTime :: m ()

environment :: MonadSim m => m Environment
environment = do
    slot <- currentSlot
    return Environment {slotInterval = SlotInterval {ivFrom = slot, ivTo = slot}}

simulate :: MonadSim m => Contract -> m ()
simulate contract = do
    traceTime
    env   <- environment
    state <- getState
    case reduceContractUntilQuiescent env state contract of
        RRAmbiguousSlotIntervalError             -> error "impossible case"
        ContractQuiescent ws ps state' contract' -> do
            setState state'
            forM_ ws (traceWarning . Left)
            forM_ ps tracePayment
            case contract' of
                Close -> return ()

                When cs t _ -> do
                    e <- pickInput (action <$> cs) t
                    case e of
                        Left slot -> do
                            slot' <- currentSlot
                            setSlot $ max slot' slot
                            simulate contract'
                        Right input -> case applyInput env state' input contract' of
                            ApplyNoMatchError -> simulate contract'
                            Applied w state'' contract'' -> do
                                traceWarning $ Right w
                                setState state''
                                simulate contract''

                _ -> error "impossible case"
  where
    action :: Case -> Action
    action (Case a _) = a

newtype SimIO a = SimIO (S.StateT (Slot, State) IO a)
    deriving (Functor, Applicative, Monad, S.MonadState (Slot, State), S.MonadIO)

instance MonadSim SimIO where

    currentSlot = S.gets fst

    getState = S.gets snd

    traceWarning e = S.liftIO $ case e of
        Left ReduceNoWarning -> return ()
        Right ApplyNoWarning -> return ()
        Left w               -> print w
        Right w              -> print w

    tracePayment = S.liftIO . print

    setSlot slot = do
        (_, state) <- S.get
        S.put (slot, state)

    setState state = do
        (slot, _) <- S.get
        S.put (slot, state)

    traceTime = do
        slot <- currentSlot
        S.liftIO $ printf "Current Slot is: %d\n" (getSlot slot)

    pickInput xs slot = do
        ys <- mapM preprocess xs
        S.liftIO $ do
            forM_ (zip [1 :: Int ..] ys) $ \(i, a) -> printf "%2d %s\n" i (show a)
            printf "%2d new slot (timeout is %d)\n" (1 + length xs) (getSlot slot)
            parse ys
      where
        preprocess :: Action -> SimIO Action
        preprocess c@(Choice _ _)  = return c
        preprocess (Deposit a r v) = do
            env   <- environment
            state <- getState
            let amount = evalValue env state v
            return $ Deposit a r $ Constant amount

        parse :: [Action] -> IO (Either Slot Input)
        parse ys = do
            s <- getLine
            let l = fromIntegral $ length ys
            case readMaybe <$> words s of
                [Just i]
                    | i >= 1 && i <= l -> case ys !! (fromIntegral i - 1) of
                        Deposit a r (Constant amount) -> return $ Right $ IDeposit a r amount
                        _                             -> parse ys
                [Just i, Just j]
                    | i >= 1 && i <= l -> case ys !! (fromIntegral i - 1) of
                        Choice cid _ -> return $ Right $ IChoice cid j
                        _            -> parse ys
                    | i == l + 1 -> return $ Left $ Slot j

                _ -> parse ys

emptyState :: State
emptyState = State
    { accounts = M.empty
    , boundValues = M.empty
    , choices = M.empty
    }

runSimIO :: SimIO a -> IO a
runSimIO (SimIO m) = S.evalStateT m (Slot 0, emptyState)

simulateIO :: Contract -> IO ()
simulateIO = runSimIO . simulate

ex1 :: Contract
ex1 = When
    [ Case (Deposit "alice" "alice" SlotIntervalStart) Close ]
    (Slot 1000)
    Close

ex2 :: Contract
ex2 = When
    [ Case (Deposit "carol" "alice" (Constant 100)) $
        When
            [ Case (Deposit "carol" "bob" (Constant 100)) $
                When
                    [ Case (Choice (ChoiceId "aliceOrBob" "carol") [Bound 0 0]) $
                        Pay "carol" (Party "alice") (Constant 200) Close
                    , Case (Choice (ChoiceId "aliceOrBob" "carol") [Bound 1 1]) $
                        Pay "carol" (Party "bob") (Constant 200) Close
                    ]
                    (Slot 1000)
                    (Pay "carol" (Party "alice") (Constant 100) $
                     Pay "carol" (Party "bob") (Constant 100) Close)
            ]
            (Slot 1000)
            (Pay "carol" (Party "alice") (Constant 100) Close)
    ]
    (Slot 1000)
    Close
