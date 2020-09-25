{-# LANGUAGE DataKinds #-}

module Phantom where

import PhantomPW

-- should only be called with a User whose password has been checked (and was correct)
withdrawMoney :: User 'Authenticated -> Int -> IO ()
withdrawMoney user amount =
    putStrLn $ "user " ++ show user ++ " is withdrawing " ++ show amount

test :: User 'Unauthenticated -> String -> Int -> IO ()
test user pw amount = do
    m <- checkPassword user pw
    case m of
        Nothing            -> putStrLn "authentication failed"
        Just authenticated -> withdrawMoney authenticated amount

