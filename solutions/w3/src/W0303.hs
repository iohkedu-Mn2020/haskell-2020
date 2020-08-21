{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE InstanceSigs   #-}

-- | W3.3 Testing Binary Trees
module W0303
    ( -- * Tree
      Tree (..)
      -- * Subtask W3.3.1
    , genTree
      -- * Subtask W3.3.2
      -- * Subtask W3.3.3
    , propFunctor
      -- * Subtask W3.3.4
    , fmap'
    ) where

import Test.QuickCheck

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Eq, Functor, Foldable)

-- Subtask W3.3.1

-- |
-- >>> length <$> generate (genTree 23 :: Gen (Tree Int))
-- 23
--
genTree :: Arbitrary a => Int -> Gen (Tree a)
genTree n
    | n <= 0    = error "Trees have at least one leaf!"
    | n == 1    = Leaf <$> arbitrary
    | otherwise = do
        nl <- elements [1 .. n - 1]
        Node <$> genTree nl <*> genTree (n - nl)

-- Subtask W3.3.2

instance Arbitrary a => Arbitrary (Tree a) where

    arbitrary :: Gen (Tree a)
    arbitrary = sized $ \s -> do
        n <- elements [0 .. s]
        if n == 0
            then Leaf <$> arbitrary
            else genTree n

    shrink :: Tree a -> [Tree a]
    shrink (Leaf a)   = Leaf <$> shrink a
    shrink (Node l r) =    [l, r]
                        ++ (flip Node r <$> shrink l)
                        ++ (Node l <$> shrink r)

-- Subtask W3.3.3

-- |
-- >>> quickCheck $ propFunctor fmap
-- +++ OK, passed 100 tests.
--
propFunctor :: ((Int -> Int) -> Tree Int -> Tree Int)
            -> Tree Int
            -> Property
propFunctor f t = f id t === t

-- Subtask W3.3.4

-- |
-- >>> quickCheck $ propFunctor fmap'
-- *** Failed! Falsified ...
-- ...
--
fmap' :: (a -> b) -> Tree a -> Tree b
fmap' f (Leaf a)   = Leaf $ f a
fmap' f (Node l r) = Node (fmap' f r) (fmap' f l)
