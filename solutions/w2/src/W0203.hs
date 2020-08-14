-- | W2.3 Unsafe IO
module W0203
    ( -- * Subtask W2.3.1
      Tree (..), relabelTree
      -- * Subtask W2.3.2
    , anything
      -- * Subtask W2.3.3
    , cast
    ) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- Subatask W2.3.1

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Read, Eq, Ord)

-- |
-- >>> relabelTree $ Node (Node (Leaf True) (Leaf False)) (Leaf True)
-- Node (Node (Leaf (True,1)) (Leaf (False,2))) (Leaf (True,3))
--
relabelTree :: Tree a -> IO (Tree (a, Int))
relabelTree t = do
    counter <- newIORef 1
    go counter t
  where
    go :: IORef Int -> Tree a -> IO (Tree (a, Int))
    go counter (Leaf a) = do
        c <- atomicModifyIORef' counter $ \n -> (n + 1, n)
        return $ Leaf (a, c)
    go counter (Node l r) = do
        l' <- go counter l
        r' <- go counter r
        return $ Node l' r'

-- Subatask W2.3.2

-- |
-- >>> writeIORef anything True >> readIORef anything :: IO Bool
-- True
--
-- >>> writeIORef anything "Haskell" >> readIORef anything :: IO String
-- "Haskell"
--
anything :: IORef a
anything = unsafePerformIO $ newIORef undefined

-- Subatask W2.3.3

-- |
-- >>> cast 'a' :: Int
-- 97
--
-- >>> cast (98 :: Int) :: Char
-- 'b'
--
-- cast True :: (Char, Char)
-- *** Segmentation fault
--
cast :: a -> b
cast a = unsafePerformIO $ do
    writeIORef anything a
    readIORef anything
