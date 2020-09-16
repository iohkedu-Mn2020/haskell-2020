{-# LANGUAGE RankNTypes #-}

module W0502
    ( BinTree (..), tree
      -- * Subtask W5.2.1
    , inorder, preorder, postorder
      -- * Subtask W5.2.2
    , printNodes, labelNodes
    ) where

import Control.Monad
import Control.Monad.State
import Optics

data BinTree a = Tip | Bin (BinTree a) a (BinTree a) deriving Show

tree :: BinTree Char         --        c
tree = Bin                   --       / \
  (Bin                       --      /   \
        (Bin Tip 'a' Tip)    --     b     d
        'b'                  --    / \   / \
        Tip)                 --   /
  'c'                        --  a
  (Bin Tip 'd' Tip)          -- / \

-- Subtask W5.2.1

-- | Traversal for traversing a @'BinTree' a@ in /in-order/ (left, value, right).
--
-- >>> toListOf inorder tree
-- "abcd"
--
inorder :: Traversal (BinTree a) (BinTree b) a b
inorder _ Tip         = pure Tip
inorder f (Bin l a r) = Bin <$> inorder f l <*> f a <*> inorder f r

-- | Traversal for traversing a @'BinTree' a@ in /pre-order/ (value, left, right).
--
-- >>> toListOf preorder tree
-- "cbad"
--
preorder :: Traversal (BinTree a) (BinTree b) a b
preorder _ Tip         = pure Tip
preorder f (Bin l a r) = (\a' l' -> Bin l' a') <$> f a <*> preorder f l <*> preorder f r

-- | Traversal for traversing a @'BinTree' a@ in /post-order/ (left, right, value).
--
-- >>> toListOf postorder tree
-- "abdc"
--
postorder :: Traversal (BinTree a) (BinTree b) a b
postorder _ Tip         = pure Tip
postorder f (Bin l a r) = (\l' r' a' -> Bin l' a' r') <$> postorder f l <*> postorder f r <*> f a

-- Subtask W5.2.1

-- | Prints the values of the given tree in order of the given traversal.
--
-- >>> printNodes postorder tree
-- 'a'
-- 'b'
-- 'd'
-- 'c'

printNodes :: Show a => Traversal' (BinTree a) a -> BinTree a -> IO ()
printNodes t = void . t (\a -> print a >> return a)

-- | Labels the nodes of the given tree in order of the given traversal, starting at 1.
--
-- >>> labelNodes preorder tree
-- Bin (Bin (Bin Tip ('a',3) Tip) ('b',2) Tip) ('c',1) (Bin Tip ('d',4) Tip)
--
labelNodes :: Traversal (BinTree a) (BinTree (a, Int)) a (a, Int) -> BinTree a -> BinTree (a, Int)
labelNodes t b = evalState (t f b) 1
  where
    f a = do
        n <- get
        put $ succ n
        return (a, n)
