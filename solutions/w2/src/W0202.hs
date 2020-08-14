-- | W2.2 Dining Philosophers
module W0202 where

import Control.Concurrent      (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad           (forever, void, forM_, replicateM)
import Prelude                 hiding (log)
import System.Environment      (getArgs)
import Text.Read               (readMaybe)

type Philosopher = Int
type Fork = Int

-- |Locks used for logging and for forks in subtasks 1 and 2.
type Lock = MVar ()

-- |Abstract logging interface.
type Log = String -> IO ()

-- |A "picking up" strategy, parametric in the fork type @a@.
type PickUp a = Log   -- ^logging facility
             -> Fork  -- ^number of the left fork
             -> Fork  -- ^number of the right fork
             -> [a]   -- ^all forks
             -> IO () -- ^picks up both forks.

-- |"Putting down" action, parametric in the fork type @a@.
type PutDown a = Fork  -- ^number of fork to put down
              -> [a]   -- ^all forks
              -> IO () -- ^puts down the fork

-- |Logs a message from the given philosopher, using the provided lock
-- to prevent garbled log messages.
log :: Lock        -- ^the logger lock
    -> Philosopher -- ^originator of the message
    -> String      -- ^log message
    -> IO ()
log l p s = withMVar l $ \_ ->
    putStrLn $ "philosopher #" ++ show p ++ ": " ++ s

-- |A philosopher, parametric in the fork type @a@.
philosopher :: Log       -- ^logging facility
            -> PickUp a  -- ^picking up strategy
            -> PutDown a -- ^putting down facility
            -> Fork      -- ^number of left fork
            -> Fork      -- ^number of right fork
            -> [a]       -- ^all forks
            -> IO ()
philosopher log' pickUp putDown left right forks = forever $ do
    log' "is hungry"
    pickUp log' left right forks
    log' "is eating"
    putDown left forks
    log' $ "dropped fork #" ++ show left
    putDown right forks
    log' $ "dropped fork #" ++ show right

-- |Dining of @n@ philosophers,
-- using locks for forks,
-- always first picking up the left fork,
-- then the right fork.
-- This will deadlock!
dine1 :: Int -> IO ()
dine1 = dine
    mkForkM
    (pickUpM (\l r -> (l, r)))
    putDownM

-- |Dining of @n@ philosophers,
-- using locks for forks,
-- always first picking up the fork with the lower number
-- then the fork with the higher number.
dine2 :: Int -> IO ()
dine2 = dine
    mkForkM
    (pickUpM (\l r -> if l <= r then (l, r) else (r, l)))
    putDownM

-- |Dining of @n@ philosophers,
-- using @`TVar`@'s for forks.
dine3 :: Int -> IO ()
dine3 = dine
    mkForkT
    pickUpT
    putDownT

-- |Dining of @n@ philosophers, paranetric in the fork type @a@
dine :: IO a      -- ^action to create a fork
     -> PickUp a  -- ^picking up strategy
     -> PutDown a -- ^putting down facility
     -> Int       -- ^number of philosophers
     -> IO ()
dine mkFork pickUp putDown n = do
    forks <- replicateM n mkFork
    lock  <- newMVar ()
    forM_ [1 .. n] $ \p -> forkIO $
        philosopher
            (log lock p)
            pickUp
            putDown
            (if p > 1 then p - 1 else n)
            p
            forks
    void getLine

-- |@`dineMain`@ takes a(n attempted) solution to the problem
-- and runs it, taking the number of philosophers as command line argument.
dineMain :: (Int -> IO ()) -> IO ()
dineMain d = do
    s <- getArgs
    case s of
        [n] -> case readMaybe n of
            Just m
                | m >= 1    -> putStrLn (show m ++ " philosopher(s)") >> d m
                | otherwise -> showError
            Nothing -> showError
        _   -> showError
  where
    showError :: IO ()
    showError = putStrLn "Please specify the number of philosophers as command line argument!"

-- |Gets the fork for a given fork number.
getFork :: Fork -- ^fork number
        -> [a]  -- ^all forks
        -> a    -- ^the fork with the given number
getFork fork forks = forks !! (fork - 1)

-- |Creates a lock(-fork) (that has not been taken).
mkForkM :: IO Lock
mkForkM = newMVar ()

-- |Creates a @`TVar`@-fork on the table.
mkForkT :: IO (TVar Bool)
mkForkT = newTVarIO False

-- |Puts down a lock-fork.
putDownM :: PutDown Lock
putDownM fork forks = putMVar (getFork fork forks) ()

-- |Puts down a @`TVar`@-fork.
putDownT :: PutDown (TVar Bool)
putDownT fork forks = atomically $ writeTVar (getFork fork forks) False

-- |Picks up a lock-fork.
pickUpM :: (Fork -> Fork -> (Fork, Fork)) -- ^given the left fork number and the right fork number, decide in which order to take them
        -> PickUp Lock
pickUpM f log' left right forks = do
    let (first, second) = f left right
    _ <- takeMVar $ getFork first forks
    log' $ "picked up fork #" ++ show first
    _ <- takeMVar $ getFork second forks
    log' $ "picked up fork #" ++ show second

-- |Picks up a @`TVar`@-fork.
pickUpT :: PickUp (TVar Bool)
pickUpT log' left right forks = do
    atomically $ do
        let lf = getFork left forks
            rf = getFork right forks
        l <- readTVar lf
        r <- readTVar rf
        case (l, r) of
            (False, False) -> writeTVar lf True >> writeTVar rf True -- if both forks are on the table, take them
            _              -> retry                                  -- otherwise retry
    log' $ "picked up forks #" ++ show left ++ " and #" ++ show right
