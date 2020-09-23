{-# LANGUAGE TemplateHaskell #-}
module Main where

-- import Criterion.Main
import Gauge.Main
import SPower

power :: Int -> Int -> Int
power n x
  | n <= 0     =  1
  | even n     =  let
                    r = power (n `div` 2) x
                  in
                    r * r
  | otherwise  =  x * power (n - 1) x

power10 :: Int -> Int
power10 x =
  let
    p10  = p5 * p5
    p5   = x * p2 * p2
    p2   = x * x
  in
    p10

pow :: Int -> Int -> Int
pow = (^)

spower10 :: Int -> Int
spower10 x = $$(spower 10 [|| x ||])

simplepower10 :: Int -> Int
simplepower10 x = $$(simplepower 10 [|| x ||])

main :: IO ()
main =
  defaultMain
    [ bench "power 10 42"  $ nf (power 10) 42
    , bench "power10 42"   $ nf power10    42
    , bench "pow 10 42"    $ nf (pow 10)   42
    , bench "spower 10 42" $ nf spower10   42
    ]
