{-# LANGUAGE BangPatterns #-}
module TailRecursion where

sum' :: [Int] -> Int
sum' = go 0
  where
    go acc []       = acc
    go acc (x : xs) =
        let! acc' = acc + x
        in   go acc' xs -- tail recursive!

sum'' :: [Int] -> Int
sum'' []       = 0
sum'' (x : xs) = x + sum'' xs -- not tail recursive!
-- (After having the result of the recursive call,
-- we still have to add x to it before we are done.)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1) -- not tail recursive!
-- (After getting factorial (n - 1), we still have
-- to multiply by n to get the final result.)

factorial' :: Integer -> Integer
factorial' = go 1
  where
    go acc 0 = acc
    go acc n = let! acc' = acc * n in go acc' (n - 1)
    -- tail recursive

factorial'' :: Integer -> Integer
factorial'' = go id
  where
    go :: (Integer -> a) -> Integer -> a
    go cont 0 = cont 1
    go cont n = go (\x -> cont (n * x)) (n - 1)
--            = go (cont . (* n))       (n - 1)
--  tail recursive - "continuation passing style
--  (CPS)"

data Tree = Tip | Node Int Tree Tree
    deriving (Show, Read, Eq, Ord)

addTree :: Tree -> Int
addTree Tip          = 0
addTree (Node n l r) = n + addTree l + addTree r
-- not tail recursive!

addTree' :: Tree -> Int
addTree' = go id
  where
    go :: (Int -> a) -> Tree -> a
    go cont Tip          = cont 0
    go cont (Node n l r) =
        go (\s -> go (\t -> cont (n + s + t)) r) l
-- tail recursive (CPS)
