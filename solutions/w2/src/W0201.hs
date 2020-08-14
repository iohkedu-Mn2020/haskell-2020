{-# LANGUAGE BangPatterns #-}

-- | W2.1 Roulette
module W0201
    ( -- * Color
      Color (..)
      -- * Subtask W2.1.1
    , roulette
      -- * Subtask W2.1.2
    , histogram
      -- * Subtask W2.1.3
    , gamblersRuin
    ) where

import           Data.Map      (Map)
import qualified Data.Map      as M
import           System.Random

data Color = Zero | Red | Black
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- Subtask W2.1.1

roulette :: IO Color
roulette = do
    n <- randomRIO (0, 36 :: Int)
    return $ if n == 0
        then Zero
        else if n <= 18
            then Red
            else Black

-- Subtask W2.1.2

-- |
-- >>> histogram 0
-- fromList []
--
histogram :: Int -> IO (Map Color Int)
histogram = go M.empty
  where
    go :: Map Color Int -> Int -> IO (Map Color Int)
    go m n
        | n <= 0    = return m
        | otherwise = do
            c <- roulette
            go (M.alter (Just . maybe 1 succ) c m) $ n - 1

-- Subtask W2.1.3

-- |
-- >>> gamblersRuin 0
-- 0
--
gamblersRuin :: Int -> IO Int
gamblersRuin = go 0
  where
    go :: Int -> Int -> IO Int
    go !acc n
        | n <= 0    = return acc
        | otherwise = do
            c <- roulette
            let !n' = case c of
                        Red -> n + 1
                        _   -> n - 1
            go (succ acc) n'
