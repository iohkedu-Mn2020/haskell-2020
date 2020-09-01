module Main
    ( main
    ) where

import Remote
import System.Environment (getArgs)

main :: IO ()
main = do
    [p] <- getArgs
    server $ read p
