{-# LANGUAGE InstanceSigs #-}

module W0503
    ( Delayed (..), loop, factorial, unsafeRunDelayed
      -- * Subtask W5.3.1
    , runDelayed
      -- * Subtask W5.3.3
    , tick, psum
      -- * Subtask W5.3.4
    , DelayedF (..), fromDelayed, toDelayed
      -- * Subtask W5.3.5
    , merge, firstSum
      -- * Subtask W5.3.6
    , biasedMerge, biasedFirstSum
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Free
import Data.Foldable       (asum)

data Delayed a = Now a | Later (Delayed a)

loop :: Delayed a
loop = Later loop

factorial :: Int -> Delayed Int
factorial = go 1
 where
  go acc n | n <= 0    = Now acc
           | otherwise = Later (go (n * acc) (n - 1))

unsafeRunDelayed :: Delayed a -> a
unsafeRunDelayed (Now   x) = x
unsafeRunDelayed (Later d) = unsafeRunDelayed d

-- Subtask W5.3.1

-- | Extracts a result from the given computation if it is guarded by at most the
-- given number of 'Later' constructors.
--
-- >>> runDelayed 1000 loop
-- Nothing
--
-- >>> runDelayed 4 (factorial 4)
-- Just 24
-- >>> runDelayed 3 (factorial 4)
-- Nothing
--
runDelayed :: Int -> Delayed a -> Maybe a
runDelayed n d
    | n < 0     = Nothing
    | otherwise = case d of
        Now a           -> Just a
        Later d'
            | n == 0    -> Nothing
            | otherwise -> runDelayed (pred n) d'

-- Subtask W5.3.2

instance Functor Delayed where
    fmap = liftM

instance Applicative Delayed where
    pure = return
    (<*>) = ap

instance Monad Delayed where

    return :: a -> Delayed a
    return = Now

    (>>=) :: Delayed a -> (a -> Delayed b) -> Delayed b
    Now a   >>= cont = cont a
    Later d >>= cont = Later $ d >>= cont

-- Subtask W5.3.3

tick :: Delayed ()
tick = Later (Now ())

-- | Delays by the length of the list, then returns the sum of the list elements.
psum :: [Int] -> Delayed Int
psum xs = sum <$> mapM (\x -> tick >> return x) xs

-- Subtask W5.3.4

data DelayedF a = LaterF a

instance Functor DelayedF where
    fmap f (LaterF a) = LaterF $ f a

fromDelayed :: Delayed a -> Free DelayedF a
fromDelayed (Now a)   = Pure a
fromDelayed (Later d) = Free $ LaterF $ fromDelayed d

toDelayed :: Free DelayedF a -> Delayed a
toDelayed (Pure a)          = Now a
toDelayed (Free (LaterF x)) = Later $ toDelayed x

-- Subtask W5.3.5

instance Alternative Delayed where
    empty = loop
    (<|>) = merge

merge :: Delayed a -> Delayed a -> Delayed a
merge (Now x)   _         = Now x
merge _         (Now   x) = Now x
merge (Later p) (Later q) = Later (merge p q)

-- | Performs 'psum' on each of the integer lists and returns the result
-- that can be obtained with as few delays as possible.
--
-- >>> runDelayed 100 $ firstSum [repeat 1, [1,2,3], [4,5], [6,7,8], cycle [5,6]]
-- Just 9
--
firstSum :: [[Int]] -> Delayed Int
firstSum = asum . map psum

-- Subtask W5.3.6

biasedMerge :: Delayed a -> Delayed a -> Delayed a
biasedMerge p q = p <|> Later q

-- | Function 'firstSum' using 'biasedMerge' instead of 'merge'.
--
-- >>> runDelayed 200 $ biasedFirstSum $ cycle [repeat 1, [1,2,3], [4,5], [6,7,8], cycle [5,6]]
-- Just 6
--
-- >>> runDelayed 200 $ biasedFirstSum $ replicate 100 (repeat 1) ++ [[1]] ++ repeat (repeat 1)
-- Just 1
--
biasedFirstSum :: [[Int]] -> Delayed Int
biasedFirstSum []         = Now 0
biasedFirstSum (xs : xss) = psum xs `biasedMerge` biasedFirstSum xss
