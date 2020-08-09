module Main
    ( main
    ) where

import IO (readLnMaybe)

main :: IO ()
main = do
    m <- readLnMaybe :: IO (Maybe Int)
    print m
