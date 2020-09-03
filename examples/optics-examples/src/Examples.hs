{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module Examples
    ( _3, leftToRight, rightToLeft, rev
    , tree
    , _Natural, _Singleton
    , Triple (..)
    , bothNames
    , labelElement
    , tree'
    ) where

import Control.Monad.State
import Numeric.Natural     (Natural)
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

{-

-- Maybe Int
-- Nothing | ... Just (-2), Just (-1), Just 0, Just 1, Just 2, ...
--         | -----------------------------------------------------
--         | ...       -2         -1        0       1       2  ...    Int

--                  s     a
_Just :: Prism' (Maybe a) a
_Just = prism' pr rv
  where
--           s       Maybe a
    pr :: Maybe a -> Maybe a   -- "matcher"
    pr (Just a) = Just a       -- pr = id
    pr Nothing  = Nothing

--        a       s
    rv :: a -> Maybe a         -- "constructor"
    rv a = Just a
-}

-- | Focuses on the natural number inside an integer.
--
-- >>> set _Natural 42 7
-- 7
-- >>> set _Natural (-3) 7
-- -3
--
-- Integer
-- ... -5, -4, -3, -2, -1 | 0, 1, 2, 3, 4, 5, ...
--
--                    s       a
_Natural :: Prism' Integer Natural
_Natural = prism'
    (\n -> if n < 0 then Nothing else Just (fromIntegral n))
    fromIntegral
{-
  where
--          s        Maybe a
    pr :: Integer -> Maybe Natural
    pr n = if n < 0 then Nothing else Just (fromIntegral n)

--           a          s
    rv :: Natural -> Integer
    rv = fromIntegral
-}

-- | Focuses on the singleton (one-element) list inside a list.
--
-- >>> set _Singleton "a" 'X'
-- "X"
-- >>> set _Singleton "" 'X'
-- ""
-- >>> set _Singleton "Haskell" 'X'
-- "Haskell"
--
-- [Bool]
-- [] | [True], [False] | [True, True], [True, False], [False, True], [False, False], [True, True, True],...
--    |  True    False
--
_Singleton :: Prism' [a] a
_Singleton = prism'
    (\xs -> case xs of
        [x] -> Just x
        _   -> Nothing
    )
    return -- (\y -> [y])

-- |
--
-- >>> each print $ Triple 1 2 3
-- 1
-- 2
-- 3
-- Triple () () ()
--
-- >>> foo n = if n < 10 then Just (2 * n) else Nothing
-- >>> each foo $ Triple 1 3 8
-- Just (Triple 2 6 16)
-- >>> each foo $ Triple 1 1 10
-- Nothing
--
data Triple a = Triple a a a
    deriving (Show, Functor, Foldable, Traversable)

data Name = Name
    { firstName :: String
    , lastName  :: String
    } deriving Show

-- | Focuses on both names.
--
-- >>> set (bothNames . each) (Name "Lars" "Bruenjes") 'X'
-- Name {firstName = "XXXX", lastName = "XXXXXXXX"}
--
--           Applicative f => (String -> f String) -> (Name -> f Name)
bothNames :: Traversal' Name String
bothNames g n = Name <$> g (firstName n) <*> g (lastName n)


-- foo       :: Traversal' s a = Applicative f => (a -> f a) -> (s -> f s)
-- bar       :: Traversal' a x = Applicative f => (x -> f x) -> (a -> f a)
-- foo . bar :: Traversal' s x = Applicative f => (x -> f x) -> (s -> f s)

-- | Labels the given element with the first element of the list in the state.
--
-- >>> runState (labelElement 'x') [11, 22, 33]
-- (('x',11),[22,33])
--
labelElement :: a -> State [Int] (a, Int)
labelElement a = do
    ls <- get
    case ls of
        []        -> error "no more labels"
        (l : ls') -> do
            put ls'
            return (a, l)

-- |
-- evalState (each labelElement tree') [1..]
-- Node (Leaf ("Haskell",1)) (Node (Leaf ("in",2)) (Leaf ("Mongolia",3)))
--
tree' :: Tree String
tree' = Node
        (Leaf "Haskell")
        (Node
            (Leaf "in")
            (Leaf "Mongolia"))
