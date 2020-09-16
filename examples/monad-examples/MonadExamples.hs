{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadExamples where

import Control.Monad.Reader
import Control.Monad.State

-- in some database application...
--

data DBConnection
data Customer
data Purchase

connect :: String -> IO DBConnection
connect connectionString = undefined

getAllCustomers :: DBConnection -> IO [Customer]
getAllCustomers conn = undefined

updateCustomer :: DBConnection -> Customer -> IO ()
updateCustomer conn c = undefined

listPurchases :: DBConnection -> Customer -> IO [Purchase]
listPurchases conn c = undefined

-- code smell: lots of function taking an argument of the same type which always will have the same value.
-- ---> Reader might be a good idea.

newtype Connected a = Connected (ReaderT DBConnection IO a)
    deriving (Functor, Applicative, Monad)

dbConnection :: Connected DBConnection
dbConnection = Connected ask

runConnected :: Connected a -> String -> IO a
runConnected (Connected m) connectionString = do
    conn <- connect connectionString
    runReaderT m conn

getAllCustomersM :: Connected [Customer]
getAllCustomersM = undefined

updateCustomerM :: Customer -> Connected ()
updateCustomerM c = undefined

listPurchasesM :: Customer -> Connected [Purchase]
listPurchasesM c = undefined

-- in a game

type Score = Int
data Monster
data Destination
data BattleResult

drinkPotion :: Score -> IO Score
drinkPotion score = undefined

fightMonster :: Score -> Monster -> IO (BattleResult, Score)
fightMonster score monster = undefined

go :: Score -> Destination -> IO (Destination, Score)
go score dest = undefined

-- code smell: lots of functions that have an argument of the same type and return an updated value of that type.
-- ---> State monad

newtype Game a = Game (StateT Score IO a)
    deriving (Functor, Applicative, Monad)

getScore :: Game Score
getScore = Game get

setScore :: Score -> Game ()
setScore = Game . put

runScore :: Game a -> IO (a, Score)
runScore (Game m) = runStateT m 0

drinkPotionM :: Game ()
drinkPotionM = undefined

fightMonsterM :: Monster -> Game BattleResult
fightMonsterM monster = undefined

goM :: Destination -> Game Destination
goM dest = undefined

-- validation

-- code smell: there are lots of things that can go wrong, so your code is littered with case-expressions or
-- if's to check for error conditions and interrupt the computation when an error occurs.
-- ---> ExceptT/ Either/ Maybe monad

-- example: Elevator DSL in T4, transaction validation in Plutus/Cardano/Bitcoin.



-- free monads
--
-- You want to support a monadic API and interpret programs written in that API in different ways (for example
-- both in IO and in a pure context).
-- ---> free monad
