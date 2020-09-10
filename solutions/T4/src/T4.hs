{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : T4
Description : fourth test for the IOHK Haskell Course in Ulaanbaatar 2020
Copyright   : (c) Lars Brünjes, 2020
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains the fourth test for the IOHK Haskell Course in Ulaanbaatar 2020.
Many doctests have been provided for your convenience. You can run them with
@cabal test@ to monitor your progress.

__Please note: From now on, we won't accept any submissions that do not compile!__
-}
module T4 where

import Control.Monad.Except  (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Free    (Free (..))
import Control.Monad.Reader  (MonadReader (..), Reader, ReaderT (..))
import Control.Monad.State   (MonadState (..), StateT (..), runState)
import Data.Char             (toUpper)
import Data.Functor.Identity (Identity (..))
import Numeric.Natural       (Natural)

import Optics

-- | A DSL for describing colors.
data Color =
      Red             -- ^ the primary color red
    | Green           -- ^ the primary color green
    | Blue            -- ^ the primary color blue
    | Mix Color Color -- ^ mixing two colors, taking the same amount from each
    deriving Show

-- | A semantic function that decides whether the given color contains some red.
--
-- >>> containsRed Red
-- True
-- >>> containsRed Green
-- False
-- >>> containsRed $ Green `Mix` Blue
-- False
-- >>> containsRed $ Green `Mix` (Blue `Mix` Red)
-- True
--
containsRed :: Color -> Bool
containsRed Red       = True
containsRed Green     = False
containsRed Blue      = False
containsRed (Mix c d) = containsRed c || containsRed d

-- | A DSL describing instructions for an elevator.
data Elevator =
      Up                    -- ^ go up one floor
    | Down                  -- ^ go down one floow
    | Seq Elevator Elevator -- ^ follow the first instructions, then the second instructions
    deriving Show

-- | Semantic function to determine the final floor an elevator will reach following the given instructions.
-- /Going down while on floor 0 or going up while on the top-most floor are __errors__./
-- You can assume that the start-floor is a valid floor, i.e. between zero and the top-most floor.
--
-- >>> destination 10 5 $ Up `Seq` Up
-- Just 7
-- >>> destination 10 0 $ Down `Seq` Up
-- Nothing
-- >>> destination 10 1 $ Down `Seq` Up
-- Just 1
-- >>> destination 20 19 $ Up `Seq` Up `Seq` Down
-- Nothing
-- >>> destination 20 18 $ Up `Seq` Up `Seq` Down
-- Just 19
--
destination :: Natural       -- ^ the top-most floor
            -> Natural       -- ^ the floor where the elevator starts
            -> Elevator      -- ^ the instructions to follow
            -> Maybe Natural -- ^ the floor after following the instructions or 'Nothing' in case of an error.
destination top start Up
    | start < top               = Just $ start + 1
    | otherwise                 = Nothing
destination _   start Down
    | start > 0                 = Just $ start - 1
    | otherwise                 = Nothing
destination top start (Seq x y) = destination top start x >>= \f -> destination top f y

-- | Write a traversal that focuses on all second components of each element of
-- both components of a pair.
-- Use the definitions from "Optics".
-- __Tip__: The solution is a simple composition of three standard optics!
--
-- >>> toListOf myTraversal ([(True, 'x'), (False, 'y')], [(True, 'z')])
-- "xyz"
--
-- >>> over myTraversal not ([("Haskell", True)], [("in", True), ("Mongolia", False)])
-- ([("Haskell",False)],[("in",False),("Mongolia",True)])
--
-- >>> set myTraversal ([('a', 'b'), ('c', 'd')], [('e', 'f'), ('g', 'h')]) 'X'
-- ([('a','X'),('c','X')],[('e','X'),('g','X')])
--
myTraversal :: Traversal ([(a, b)], [(a, b)]) ([(a, b')], [(a, b')]) b b'
myTraversal = both . each . _2

-- | Binary trees.
data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Functor, Foldable, Traversable)

-- | Use optics to write a function that changes the first component of each
-- leaf in a tree to upper case.
--
-- >>> import Data.Char (toUpper)
-- >>> firstToUpper $ Leaf ('a', 42)
-- Leaf ('A',42)
-- >>> firstToUpper $ Node (Leaf ('q', True)) (Leaf ('u', False))
-- Node (Leaf ('Q',True)) (Leaf ('U',False))
-- >>> firstToUpper $ Node (Node (Leaf ('h', "Haskell")) (Leaf ('y', "in"))) (Leaf ('z', "Mongolia"))
-- Node (Node (Leaf ('H',"Haskell")) (Leaf ('Y',"in"))) (Leaf ('Z',"Mongolia"))
--
firstToUpper :: Tree (Char, a) -> Tree (Char, a)
firstToUpper = over (each . _1) toUpper

-- | The type of operations for the free Ask-monad.

data AskOp a = Ask (String -> a)
    deriving Functor

-- | A free monad with one operation.
type Ask = Free AskOp

-- | Helper function to make working with the Ask-monad easier.
myAsk :: Ask String
myAsk = Free $ Ask return

-- | An example computation in the free Ask-monad.
testAsk :: Ask String
testAsk = do
    s <- myAsk
    return $ s ++ s

-- | Write an interpreter that interprets a computation from the free
-- Ask-monad in a standard reader monad.
--
-- >>> import Control.Monad.Reader (runReader)
-- >>> runReader (askToReader testAsk) "Haskell"
-- "HaskellHaskell"
--
askToReader :: Ask a -> Reader String a
askToReader (Pure a)       = return a
askToReader (Free (Ask k)) = ask >>= askToReader . k

-- | A custom monad built using monad transformers.
newtype M a = M (ReaderT Bool (ExceptT String (StateT Int Identity)) a)
    deriving (Functor, Applicative, Monad, MonadReader Bool, MonadError String, MonadState Int)

-- | An example computation in the 'M'-monad.
testM :: M Double
testM = do
    b <- ask
    n <- get
    put $ if b then n + 1 else n + 2
    if n == 0
        then throwError "division by zero"
        else return $ 1 / fromIntegral n

-- | Write a "runner" for the 'M'-monad.
-- __Tip__: Use a suitable combination of the standard "runners" 'runReaderT', 'runExceptT' and, 'runState'.
--
-- >>> runM testM True 4
-- (Right 0.25,5)
--
-- >>> runM testM False 8
-- (Right 0.125,10)
--
-- >>> runM testM True 0
-- (Left "division by zero",1)
--
-- >>> runM testM False 0
-- (Left "division by zero",2)
--
runM :: M a                    -- ^ The computation to run.
     -> Bool                   -- ^ The read-only 'ReaderT' state.
     -> Int                    -- ^ The initial 'StateT' state.
     -> (Either String a, Int) -- ^ First component is a 'Left' error message or 'Right' results, second is the new state.
runM (M m) b = runState (runExceptT $ runReaderT m b)
