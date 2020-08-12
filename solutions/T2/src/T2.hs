{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : T2
Description : second test for the IOHK Haskell Course in Ulaanbaatar 2020
Copyright   : (c) Lars Brünjes, 2020
License     : MIT
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains the second test for the IOHK Haskell Course in Ulaanbaatar 2020.
Many doctests have been provided for your convenience. You can run them with
@cabal test@ to monitor your progress.
-}
module T2 where

import           Control.Concurrent.STM
import           Control.Monad          (replicateM_)
import           Data.Map               (Map)
import qualified Data.Map               as M

-- |Write a function 'triangle' that takes an 'Int' @n@ as input and prints
-- @n@ lines of stars to the console, where the @i@-th line has @i@ stars.
-- For negative or zero @n@, nothing is printed.
--
-- >>> triangle 4
-- *
-- **
-- ***
-- ****
--
-- >>> triangle 6
-- *
-- **
-- ***
-- ****
-- *****
-- ******
--
triangle :: Int -> IO ()
triangle n = mapM_ f [1 .. n]
  where
    f :: Int -> IO ()
    f i = replicateM_ i (putChar '*') >> putStrLn ""

-- |Consider the following function 'tripleM':
tripleM :: Monad m => m Int -> m Int -> m Int -> m Int
tripleM ma mb mc = do
    a <- ma
    b <- mb
    c <- mc
    return $ f a b c
 where
   f x y z = x + y * z

-- |Implement function 'tripleA' to behave exactly as 'tripleM' does
-- (for monads @m@), i.e. change the do-block into applicative style.
-- All you have to do is copy-paste the definition of 'tripleM',
-- then change the do-block into one line using '<$>' and '<*>'.
--
-- >>> tripleA (Just 1) (Just 2) (Just 3)
-- Just 7
--
-- >>> tripleA [1, 2] [3] [4]
-- [13,14]
--
-- >>> tripleA (Right 7) (Left "Foo") (Left "Bar")
-- Left "Foo"
--
tripleA :: Applicative f => f Int -> f Int -> f Int -> f Int
tripleA a b c = f <$> a <*> b <*> c
  where
    f x y z = x + y * z

-- |Consider the type @'NonEmpty' a@ of /non-empty lists/ with
-- elements of type @a@.
-- So we represent a non-empty list by its head and its (possibly empty)
-- tail, which is an ordinary list. The non-empty list @[1,2,3]@, for example,
-- would be represented as @NE 1 [2,3]@.
data NonEmpty a = NE a [a]
    deriving (Show, Read, Eq, Ord)

-- |Write a 'Functor' instance for 'NonEmpty'.
--
-- >>> import Data.Char (toUpper)
-- >>> toUpper <$> NE 'h' "ello"
-- NE 'H' "ELLO"
--
-- >>> succ <$> NE 1 [2 .. 10]
-- NE 2 [3,4,5,6,7,8,9,10,11]
--
instance Functor NonEmpty where

    fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
    fmap f (NE x xs) = NE (f x) (map f xs)

-- |Write a 'Foldable' instance for 'NonEmpty'.
--
-- >>> import Data.Foldable (toList)
-- >>> toList $ NE 'M' "ongolia"
-- "Mongolia"
--
-- >>> sum $ NE 1 [2, 3, 4]
-- 10
--
-- >>> null $ NE True []
-- False
--
instance Foldable NonEmpty where

    foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    foldr f b (NE x xs) = f x (foldr f b xs)

-- | Write a function 'insert' that inserts a value @v@ for a key @k@
-- into a 'Map' from keys to /lists/ of values by inserting a new singleton
-- list containing just @v@ if the key is not already in the map
-- and by prepending @v@ to the existing list of values for @k@ otherwise.
--
-- >>> insert 'x' 1 M.empty
-- fromList [('x',[1])]
--
-- >>> insert 'y' 2 $ M.fromList [('x', [1]), ('y', [3])]
-- fromList [('x',[1]),('y',[2,3])]
--
-- >>> insert 'y' 2 $ M.fromList [('x', [1])]
-- fromList [('x',[1]),('y',[2])]
--
insert :: Ord k => k -> v -> Map k [v] -> Map k [v]
insert k v = M.alter f k
  where
    f Nothing   = Just [v]
    f (Just vs) = Just $ v : vs

-- |Implement a function 'swap' that takes two 'TVar's and swaps their
-- values in a single transaction.
--
-- >>> vx <- newTVarIO 1
-- >>> vy <- newTVarIO 2
-- >>> atomically $ swap vx vy
-- >>> (,) <$> readTVarIO vx <*> readTVarIO vy
-- (2,1)
--
swap :: TVar a -> TVar a -> STM ()
swap v w = do
    x <- readTVar v
    y <- readTVar w
    writeTVar v y
    writeTVar w x
