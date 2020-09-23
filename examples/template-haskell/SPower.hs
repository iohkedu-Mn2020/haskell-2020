{-# LANGUAGE TemplateHaskell #-}
module SPower where

import Language.Haskell.TH

type Code a = Q (TExp a)

spower :: Int -> Code Int -> Code Int
spower n x
  | n <= 0     =  [|| 1 ||]
  | even n     =  [|| let
                        r = $$(spower (n `div` 2) x)
                      in
                        r * r
                  ||]
  | otherwise  =  [|| $$x * $$(spower (n - 1) x) ||]

simplepower :: Int -> Code Int -> Code Int
simplepower n x
  | n <= 0     =  [|| 1 ||]
  | otherwise  =  [|| $$x * $$(simplepower (n - 1) x) ||]

