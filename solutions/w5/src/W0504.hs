{-# LANGUAGE RankNTypes #-}

module W0504
    ( heading, tailing, taking, dropping, filtering, element
    -- * Useful helper functions
    , trans1, trans2, trans3
    ) where

import Control.Monad.State
import Data.Functor.Compose (Compose (..))
import Optics

-- | Focuses on the first element of the given traversal's focuses.
--
-- >>> set (heading each) "Ulaanbaatar" 'x'
-- "xlaanbaatar"
-- >>> set (heading each) "" 'x'
-- ""
--
heading :: Traversal' s a -> Traversal' s a
heading = taking 1

-- | Focuses on the same elements as the given traversal, skipping the first one.
--
-- >>> set (tailing each) "Ulaanbaatar" 'x'
-- "Uxxxxxxxxxx"
-- >>> set (tailing each) "y" 'x'
-- "y"
-- >>> set (tailing each) "" 'x'
-- ""
--
tailing :: Traversal' s a -> Traversal' s a
tailing t f s = evalState (getCompose (t (trans1 f) s)) False

-- | Keeps the given number of focuses from the given traversal.
--
-- >>> set (taking 3 each) "Ulaanbaatar" 'x'
-- "xxxanbaatar"
-- >>> set (taking 3 each) "yy" 'x'
-- "xx"
--
taking :: Int -> Traversal' s a -> Traversal' s a
taking n t f s = evalState (getCompose (t (trans2 f) s)) n

-- | Drops the given number of focuses from the given traversal.
--
-- >>> set (dropping 3 each) "Ulaanbaatar" 'x'
-- "Ulaxxxxxxxx"
-- >>> set (dropping 3 each) "yy" 'x'
-- "yy"
--
dropping :: Int -> Traversal' s a -> Traversal' s a
dropping n t
    | n <= 0    = t
    | otherwise = dropping (n - 1) $ tailing t

-- | Keeps on those focuses of the given traversal
-- that satisfy the given property.
--
-- >>> set (filtering (<'d') each) "Ulaanbaatar" 'x'
-- "xlxxnxxxtxr"
--
filtering :: (a -> Bool) -> Traversal' s a -> Traversal' s a
filtering p t f = t (trans3 p f)

-- | Focuses on the focus of the given traversal with the given zero-based index.
--
-- >>> set (element 1 each) "Ulaanbaatar" 'x'
-- "Uxaanbaatar"
-- >>> set (element 100 each) "Ulaanbaatar" 'x'
-- "Ulaanbaatar"
--
element :: Int -> Traversal' s a -> Traversal' s a
element i t
    | i < 0     = \_ s -> pure s
    | otherwise = heading $ dropping i t

-- Helper functions with the following signatures might be useful:

trans1 :: Applicative f => (a -> f a) -> (a -> Compose (State Bool) f a)
trans1 f a = Compose $ do
    b <- get
    put True
    return $ if b then f a else pure a

trans2 :: Applicative f => (a -> f a) -> (a -> Compose (State Int) f a)
trans2 f a = Compose $ do
    n <- get
    put $ max 0 $ n - 1
    return $ if n > 0 then f a else pure a

trans3 :: Applicative f => (a -> Bool) -> (a -> f a) -> (a -> f a)
trans3 p f a
    | p a       = f a
    | otherwise = pure a
