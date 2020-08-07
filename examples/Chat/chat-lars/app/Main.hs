module Main where

import Chat
import System.Environment (getArgs)

main :: IO ()
main = do
    [p] <- getArgs
    run $ read p
