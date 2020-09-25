{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module PhantomPW
    ( User
    , mkUser
    , UserState (..)
    , checkPassword
    ) where

-- import Control.Monad

-- newtype Const a b = Const {getConst :: a}

data User (a :: UserState) = User
    { userName :: String
    , userAge  :: Int
    } deriving Show

mkUser :: String -> Int -> User 'Unauthenticated
mkUser = User

data UserState = Authenticated | Unauthenticated
--data Authenticated
--data Unauthenticated

-- returns True if the password is correct, False otherwise
checkPassword :: User 'Unauthenticated -> String -> IO (Maybe (User 'Authenticated))
checkPassword user pw = do
    putStrLn $ "authenticating user " ++ show user
    if pw == "Haskell" then return $ Just $ User (userName user) (userAge user)
                       else putStrLn "wrong password" >> return Nothing
