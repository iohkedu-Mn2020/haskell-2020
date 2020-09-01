{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}

module Examples
    ( _3, leftToRight, rightToLeft, rev
    , tree
    ) where

import Optics

--       ------------                ------------
--       - s        -                - t        -
--       -          -                -          -
--       -          -                -          -
--       ------------                ------------
--       - a | a |  -   -----------> - b | b |  -
--       - ----------      a -> b    ------------

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Functor, Foldable)

instance Traversable Tree where
    traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse g (Leaf a)   = Leaf <$> g a
    traverse g (Node l r) = Node <$> traverse g l <*> traverse g r

-- s = Tree a  a = a
-- t = Tree b  b = b

-- | Traverses leaves left to right.
--
-- >>> toListOf leftToRight tree
-- [Just (True,"Haskell"),Nothing,Just (False,"Mongolia")]
--
leftToRight :: Traversal (Tree a) (Tree b) a b
leftToRight = traverse

-- | Traverses leaves right to left.
--
-- >>> toListOf rightToLeft tree
-- [Just (False,"Mongolia"),Nothing,Just (True,"Haskell")]
--
-- >>> (rightToLeft . _Just . _2 . each) print tree
-- 'M'
-- 'o'
-- 'n'
-- 'g'
-- 'o'
-- 'l'
-- 'i'
-- 'a'
-- 'H'
-- 'a'
-- 's'
-- 'k'
-- 'e'
-- 'l'
-- 'l'
-- Node (Leaf (Just (True,[(),(),(),(),(),(),()]))) (Node (Leaf Nothing) (Leaf (Just (False,[(),(),(),(),(),(),(),()]))))
--
-- >>> import Data.Char
-- >>> over (rightToLeft . _Just . _2 . each) toUpper tree
-- Node (Leaf (Just (True,"HASKELL"))) (Node (Leaf Nothing) (Leaf (Just (False,"MONGOLIA"))))
--
rightToLeft :: Traversal (Tree a) (Tree b) a b
rightToLeft g (Leaf a)   = Leaf <$> g a
rightToLeft g (Node l r) = flip Node <$> rightToLeft g r <*> rightToLeft g l

tree :: Tree (Maybe (Bool, String))
tree = Node
        (Leaf (Just (True, "Haskell")))
        (Node
            (Leaf Nothing)
            (Leaf (Just (False, "Mongolia"))))


-- s = (a, b, c)  a = c
-- t = (a, b, d)  b = d

-- | Focuses on the third component of a triple.
--
-- >>> set _3 (42, True, "Haskell") 'X'
-- (42,True,'X')
--
-- >>> set (_3 . each) (42, True, "Haskell") 'X'
-- (42,True,"XXXXXXX")
--
_3 :: Lens (a, b, c) (a, b, d) c d
_3 = lens gt st
  where
    gt (_, _, a) = a
    st (n, s, _) b = (n, s, b)

-- s = (a, b, c)  a = (c, b, a)
-- t = (a, b, d)  b = (d, b, a)

-- (Int, String, a)
--     \   |   /
--         *
--     /   |  \
-- (b  , String, Int)

-- | Focuses on the flipped tripple.
--
-- >>> view _3 (1, 2, 3)
-- 3
--
-- >>> view (rev . _3) (1, 2, 3)
-- 1
--
-- set (rev . _3 . _Just . each) ("Haskell", 42, False) 'X'
-- ("XXXXXXX",42,False)
--
rev :: Iso (a, b, c) (a, b, d) (c, b, a) (d, b, a)
rev = iso vw rv
  where
    vw (n, s, c) = (c, s, n)
    rv (d, s, n) = (n, s, d)
