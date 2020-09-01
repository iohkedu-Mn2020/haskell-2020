module Main
    ( main
    ) where

import Client
import System.Environment (getArgs)

main :: IO ()
main = do
    [p] <- getArgs
    client $ read p
