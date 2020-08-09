{-# OPTIONS_GHC -Wno-unused-imports #-}
module Datatypes where

-- We hide functions we are going to redefine.
import Prelude hiding ((++), or, reverse, filter)
import Control.Applicative ((<|>), liftA2)

-- Task Datatypes-1.
--
-- Define logical implication, which is given
-- by the following truth table (first argument
-- to the left, second argument on the top)
--
--       | False | True
-- ------+-------+------
-- False | True  | True
-- True  | False | True
--
-- Try to make the definition as lazy as possible.
-- Do not use other functions.

-- |
-- >>> implies False False
-- True
-- >>> implies True False
-- False
--
implies :: Bool -> Bool -> Bool
implies False _ = True
implies True  b = b

-- Task Datatypes-2.
--
-- Define logical implication again, this time in terms
-- of 'not' and '||', both of which are predefined.

implies' :: Bool -> Bool -> Bool
implies' x y = y || not x

-- Task Datatypes-3.
--
-- Reimplement 'orelse' from the slides.

-- |
-- >>> Nothing `orelse` Just 'x'
-- Just 'x'
-- >>> Just 2 `orelse` Just 3
-- Just 2
--
orelse :: Maybe a -> Maybe a -> Maybe a
orelse m@(Just _) _ = m
orelse _          n = n

-- Task Datatypes-4.
--
-- Reimplement 'mapMaybe' from the slides.

-- |
-- >>> mapMaybe (+ 2) (Just 6)
-- Just 8
--
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f (Just x) = Just (f x)
mapMaybe _ Nothing  = Nothing

-- Task Datatypes-5.
--
-- In GHCi, check the existing operator (<|>), actually
-- imported from Control.Applicative in the module header.
--
-- Observe what type it has. Observe that it behaves
-- exactly the same on the examples from the slides
-- as the 'orelse' function.

-- Task Datatypes-6.
--
-- In GHCi, check the existing operator (<$>).
--
-- Observe what type it has. Observe that it behaves
-- exactly the same on the examples from the slides
-- as the 'mapMaybe' function.

-- Task Datatypes-7.
--
-- Implement a function that evaluates two 'Maybe's,
-- and if both are 'Just', returns their elements
-- as a pair. Otherwise, it return 'Nothing'.
--
-- Define the function without using other functions.

-- |
-- >>> pairMaybe (Just 'x') (Just 'y')
-- Just ('x','y')
--
-- >>> pairMaybe (Just 42) Nothing
-- Nothing
--
pairMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
pairMaybe (Just x) (Just y) = Just (x, y)
pairMaybe _        _        = Nothing

-- Task Datatypes-8.
--
-- Reimplement the function 'liftMaybe' from the slides.

-- |
-- >>> liftMaybe (+) (Just 2) (Just 3)
-- Just 5
--
-- >>> liftMaybe (*) (Just 7) Nothing
-- Nothing
--
liftMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe f (Just a) (Just b) = Just (f a b)
liftMaybe _ _        _        = Nothing

-- Task Datatypes-9.
--
-- Reimplement 'pairMaybe' using 'liftMaybe'.

pairMaybe' :: Maybe a -> Maybe b -> Maybe (a, b)
pairMaybe' = liftMaybe (,)

-- Task Datatypes-10.
--
-- Reimplement 'addMaybes' from the slides, but
-- by using 'liftMaybe'.

-- |
-- >>> addMaybes (Just 7) (Just 3)
-- Just 10
--
-- >>> addMaybes Nothing (Just 0)
-- Nothing
--
addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes = liftMaybe (+)

-- Task Datatypes-11.
--
-- Comment out the type signature of 'addMaybes'
-- above. Then reload the file into GHCi and let
-- GHCi infer the type of 'addMaybes'.
--
-- See if it works if you use the function with
-- fractional numbers.

-- Task Datatypes-12.
--
-- Reimplement 'addMaybes' from the slides, this
-- time using 'pairMaybe', 'uncurry', and
-- 'mapMaybe'.

addMaybes' :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes' x y = mapMaybe (uncurry (+)) (pairMaybe x y)

-- Task Datatypes-13.
--
-- In GHCi, check the existing function 'liftA2'.
--
-- Observe what type it has. Observe that it behaves
-- exactly the same on 'Maybe' as 'liftMaybe'; for
-- example, by replacing 'liftMaybe' by 'liftA2'
-- in the definition of the 'addMaybe'.

-- Task Datatypes-14.
--
-- Define a function that applies both the given
-- functions to the given value and returns the
-- results as a pair.

-- |
-- >>> split (+ 1) (* 2) 7
-- (8,14)
--
split :: (a -> b) -> (a -> c) -> a -> (b, c)
split f g a = (f a, g a)

-- Task Datatypes-15.
--
-- Reimplement '++' from the slides.

-- |
-- >>> [1,2] ++ [9,10]
-- [1,2,9,10]
--
-- >>> "Mongolia" ++ "Haskell"
-- "MongoliaHaskell"
--
(++) :: [a] -> [a] -> [a]
(++) [] ys       = ys
(++) (x : xs) ys = x : (xs ++ ys)

-- Task Datatypes-16.
--
-- Implement a function 'or' that checks whether
-- a least one element in the given list of 'Bool's
-- is 'True'.

-- |
-- >>> or []
-- False
--
-- >>> or [False,True,False]
-- True
--
or :: [Bool] -> Bool
or []           = False
or (True : _)   = True
or (False : bs) = or bs

-- Task Datatypes-17.
--
-- Reimplement the function 'reverse' from the slides.

reverse :: [a] -> [a]
reverse []       = []
reverse (x : xs) = reverse xs ++ [x]

-- Task Datatypes-18.
--
-- Implement a function that takes two lists and
-- returns the reversed second list concatenated with
-- the first list.
--
-- Do NOT use 'reverse' and '++' to define this function.
-- Do it directly, by applying the standard design
-- pattern on lists.
--

-- |
-- >>> reverseAcc "Mongolia" "Haskell"
-- "lleksaHMongolia"
--
reverseAcc :: [a] -> [a] -> [a]
reverseAcc acc []       = acc
reverseAcc acc (x : xs) = reverseAcc (x : acc) xs

-- Task Datatypes-19.
--
-- One way to look at the previous task is that the first
-- argument is the accumulator, that is initially empty,
-- and while traversing the list, contains the reversed
-- list that we have seen so far.
--
-- Observe that the reimplemented reverse below indeed
-- reverses a list.

reverse' :: [a] -> [a]
reverse' = reverseAcc []

-- Task Datatypes-20.
--
-- Observe that '[1 .. 10]' produces a list containing
-- the numbers from '1' up to '10'.
--
-- Use this notation to generate lists of various lengths,
-- and then use the two versions of 'reverse' to reverse
-- them.
--
-- Do you observe one of the two versions to be faster
-- than the other?

-- Task Datatypes-21.
--
-- Reimplement 'filter' from the slides.

-- |
-- >>> filter even [1 .. 7]
-- [2,4,6]
--
filter :: (a -> Bool) -> [a] -> [a]
filter _ []       = []
filter p (x : xs)
    | p x         = x : filter p xs
    | otherwise   = filter p xs

-- Task Datatypes-22.
--
-- The predefined function 'mod' performs an integer
-- division and returns the remainder of the division.
--
-- In particular, 'mod' returns 0 if and only if the
-- first argument is divisible by the second.
--
-- Use 'mod', 'filter', and the '[1 .. n]' construction
-- to compute all the divisors of a given integer.

-- |
-- >>> divisors 24
-- [1,2,3,4,6,8,12,24]
--
divisors :: Integral a => a -> [a]
divisors n = filter (\x -> mod n x == 0) [1 .. n]

-- Task Datatypes-23.
--
-- The type class 'Integral' contains both 'Int',
-- a 64-bit integer type (assuming a 64-bit architecture),
-- and 'Integer', a memory-bounded 'Integer' type.
--
-- Use divisors on a really large number, such as
--
-- 1000000000000000000000
--
-- and verify that in principle, it works, and that lazy
-- evaluation produces parts of the list, even though you
-- probably cannot wait for the computation to finish.
--
-- You can interrupt the computation using Ctrl-C.

-- Task Datatypes-24.
--
-- A naive check whether a number is prime checks whether
-- its divisors are just 1 and the number itself.
-- Implement this check.

-- |
-- >>> isPrime 2
-- True
--
-- >>> isPrime 1
-- False
--
-- >>> isPrime 24
-- False
--
-- >>> isPrime 101
-- True
--
isPrime :: Integral a => a -> Bool
isPrime n = divisors n == [1, n]

-- Task Datatypes-25.
--
-- What happens if you type in '[1 ..]' into GHCi?

-- Task Datatypes-26.
--
-- Figure out what the pre-defined function 'take' does.

-- Task Datatypes-27.
--
-- Compute the first 1000 prime numbers.

-- |
-- >>> length thousandPrimes
-- 1000
--
-- >>> all isPrime thousandPrimes
-- True
--
thousandPrimes :: [Int]
thousandPrimes = take 1000 $ filter isPrime [1 ..]

-- After computing 'thousandPrimes' in GHCi once, compute
-- it a second time. What do you observe?

-- GO TO Tables.hs

-- RETURN HERE from Transactions.hs

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)

tree1 :: Tree Int
tree1 = Leaf 1

tree2 :: Tree Int
tree2 = Node (Leaf 2) (Leaf 4)

tree3 :: Tree Int
tree3 = Node tree2 tree1

tree4 :: Tree Int
tree4 = Node tree2 tree3

-- Task Datatypes-28.
--
-- Draw a picture (on paper) of 'tree4'.

-- Task Datatypes-29.
--
-- Re-implement 'height' from the slides.

-- |
-- >>> map height [tree1, tree2, tree3, tree4]
-- [0,1,2,3]
--
height :: Tree a -> Int
height (Leaf _)   = 0
height (Node l r) = 1 + max (height l) (height r)

-- Task Datatypes-30.
--
-- Implement a map function on trees. It
-- should produce a tree of the same shape
-- as the original tree, where the elements
-- in the new tree have been transformed by
-- the function.

-- |
-- >>> mapTree (+ 1) tree2
-- Node (Leaf 3) (Leaf 5)
--
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a)   = Leaf (f a)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

-- Task Datatypes-31.
--
-- Check whether two trees have the same
-- shape. Implement this directly, without
-- using other functions.

-- |
-- sameShape tree1 tree2
-- False
--
-- sameShape tree3 (mapTree (* 10) tree3)
-- True
--
sameShape :: Tree a -> Tree b -> Bool
sameShape (Leaf _)   (Leaf _)     = True
sameShape (Node l r) (Node l' r') = sameShape l l' && sameShape r r'
sameShape _          _            = False

-- Task Datatypes-32.
--
-- Re-implement 'sameShape', but this time,
-- use the derived equality on trees and
-- 'mapTree'. Also use the pre-defined
-- "unit" datatype, which is a datatype with
-- just one value. It has special syntax and
-- can be thought to be defined via
--
--   data () = ()
--
-- The single constructor '()' is called
-- "unit" as well:
--
--   () :: ()

sameShape' :: Tree a -> Tree b -> Bool
sameShape' x y = f x == f y
  where
    f = mapTree (const ())

-- Task Datatypes-33.
--
-- Build a complete tree of the given height
-- (i.e., a tree where all leaves are at the
-- same depth from the root).
--
-- If the given integer is zero or negative,
-- return just a single leaf.

-- |
-- >>> buildTree (-17)
-- Leaf ()
--
-- >>> buildTree 2
-- Node (Node (Leaf ()) (Leaf ())) (Node (Leaf ()) (Leaf ()))
--
buildTree :: Int -> Tree ()
buildTree h
    | h <= 0    = Leaf ()
    | otherwise = let x = buildTree (h - 1) in Node x x

-- Task Datatypes-34.
--
-- Write a function that takes a tree of
-- trees and turns it into a single tree
-- by replacing each leaf with the tree
-- contained in that leaf.

-- |
-- >>> graft (Node (Leaf (Leaf 'x')) (Leaf (Node (Leaf 'y') (Leaf 'z'))))
-- Node (Leaf 'x') (Node (Leaf 'y') (Leaf 'z'))
--
graft :: Tree (Tree a) -> Tree a
graft (Leaf t)   = t
graft (Node l r) = Node (graft l) (graft r)

-- Task Datatypes-35.
--
-- Explain in words what the following
-- function does.

function :: Tree Int -> Tree ()
function t = graft (mapTree buildTree t)

-- Task Datatypes-36.
--
-- Re-implement the 'eval' function on expressions
-- from the slides.

data Expr =
    Lit Int
  | Add Expr Expr
  | Neg Expr
  | IfZero Expr Expr Expr
  deriving (Eq, Show)

expr1 :: Expr
expr1 = Neg (Add (Lit 3) (Lit 5))

expr2 :: Expr
expr2 = IfZero expr1 (Lit 1) (Lit 0)

eval :: Expr -> Int
eval (Lit n)        = n
eval (Add x y)      = eval x + eval y
eval (Neg x)        = - (eval x)
eval (IfZero x y z) = eval (if eval x == 0 then y else z)

prop_eval1 :: Bool
prop_eval1 = eval expr1 == -8

prop_eval2 :: Bool
prop_eval2 = eval expr2 == 0

-- Task Datatypes-37.
--
-- Implement a function that constructs
-- an expression that subtracts one expression
-- from another.

-- |
-- >>> sub (Lit 42) (Lit 2)
-- Add (Lit 42) (Neg (Lit 2))
--
sub :: Expr -> Expr -> Expr
sub x y = Add x (Neg y)

-- Task Datatypes-38.
--
-- Implement a function that counts the number
-- of operations in an expression. All of 'Add',
-- 'Neg', and 'IfZero' count as one operation.

-- |
-- >>> countOps (Lit 3)
-- 0
--
-- >>> countOps (Add (Lit 7) (Neg (Lit 5)))
-- 2
--
countOps :: Expr -> Int
countOps (Lit _)        = 0
countOps (Add x y)      = 1 + countOps x + countOps y
countOps (Neg x)        = 1 + countOps x
countOps (IfZero x y z) = 1 + countOps x + countOps y + countOps z

-- Task Datatypes-39.
--
-- Implement a function that produces a textual
-- representation of an expression in a suitable
-- concrete syntax.
--
-- For example, the representation of expr2 should
-- be something like
--
--   ifzero - (3 + 5) then 1 else 0
--
-- Use as many parentheses as you need or want.
-- You do not have to try to minimize the number
-- of parentheses needed.

showExpr :: Expr -> String
showExpr (Lit n)
    | n >= 0            = show n
    | otherwise         = '(' : show n ++ ")"
showExpr (Add x y)      = '(' : showExpr x ++ " + " ++ showExpr y ++ ")"
showExpr (Neg x)        = "(- " ++ showExpr x ++ ")"
showExpr (IfZero x y z) = "(if " ++ showExpr x ++ " == 0 then " ++ showExpr y ++ " else " ++ showExpr z ++ ")"

-- Task Datatypes-40.
--
-- Add a constructor 'Mul' for multiplication to
-- the expression language and adapt all functions
-- accordingly.

