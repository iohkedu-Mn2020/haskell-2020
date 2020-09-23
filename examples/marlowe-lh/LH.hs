module LH where

import Prelude hiding (take)

{-@ xx :: { i : Integer | i >= 1 } @-}
xx :: Integer
xx = 42

{-@ y :: { i : Integer | i >= 3 } @-}
y :: Integer
y = xx + xx + 1

{-@ f :: { z : Integer | z >= 2 } -> { i : Integer | i == 1 + 2 * z } @-}
f :: Integer -> Integer
f z = z + z + 1

data Chain = GenesisBlock | Block Chain Int

{-@ measure chainLength @-}
{-@ type MyNat = { i : Int | i >= 0 } @-}
{-@ chainLength :: Chain -> MyNat @-}
chainLength :: Chain -> Int
chainLength GenesisBlock = 0
chainLength (Block c _)  = 1 + chainLength c

{-@ measure chainIsGenesis @-}
chainIsGenesis :: Chain -> Bool
chainIsGenesis GenesisBlock = True
chainIsGenesis (Block _ _)  = False

{-@ exampleChain :: { c : Chain | chainLength c == 2 } @-}
exampleChain :: Chain
exampleChain = Block (Block GenesisBlock 2) 7

-- GenesisBlock :: { c : Chain | chainLength c == 0 && chainIsGenesis c == True }
-- Block :: c : Chain -> Int -> { d : Chain | chainLength d == 1 + chainLength c && chainIsGenesis d == False }

{-@ exampleList :: { xs : [Int] | len xs == 2 } @-}
exampleList :: [Int]
exampleList = 2 : 7 : []

{-@ predicate Between X L U = (L <= X && X <= U) || (U <= X && X <= L) @-}
{-@ predicate Min X Y Z = if Y <= Z then X == Y else X == Z @-}

{-@ inline mymin @-}
mymin :: Int -> Int -> Int
mymin x z = if x <= z then x else z

{-@ take :: n : Nat -> xs : [a] -> { rs : [a] | len rs == mymin n (len xs) } @-}
take :: Int -> [a] -> [a]
take  n _xs
  | n <= 0       = []
take _n []       = []
take  n (x : xs) = x : take (n - 1) xs

{-@ restrictiveTake :: n : Nat -> { xs : [a] | len xs >= n } -> { rs : [a] | len rs == n } @-}
restrictiveTake :: Int -> [a] -> [a]
restrictiveTake = take



