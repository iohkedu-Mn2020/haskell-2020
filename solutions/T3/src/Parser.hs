module Parser
    ( Parser (..)
    , eof
    , satisfy
    , token
    ) where

import           Control.Applicative
import           Control.Monad

newtype Parser t a = Parser {runParser :: [t] -> [(a, [t])]}

instance Functor (Parser t) where

    fmap = liftM

instance Applicative (Parser t) where

    pure = return

    (<*>) = ap

instance Monad (Parser t) where

    return a = Parser $ \ts -> [(a, ts)]

    p >>= cont = Parser $ \ts -> do
        (a, ts') <- runParser p ts
        runParser (cont a) ts'

instance Alternative (Parser t) where

    empty = Parser $ const []

    p <|> q = Parser $ \st -> runParser p st ++ runParser q st

instance MonadPlus (Parser t) where

    mzero = empty

    mplus = (<|>)

eof :: Parser t ()
eof = Parser $ \ts -> case ts of
    [] -> [((),[])]
    _  -> []

satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser $ \ts -> case ts of
    []              -> []
    (x : xs)
        | p x       -> [(x, xs)]
        | otherwise -> []

token :: Eq t => t -> Parser t ()
token t = () <$ satisfy (== t)
