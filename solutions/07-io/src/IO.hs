{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}

module IO where

-- We once again hide functions we're going to reimplement.
-- However, some of the exercises may require you to add
-- additional imports.
import Prelude               hiding (readLn)
import Control.Exception     (bracket)
import Control.Monad         (liftM, liftM2)
import Data.ByteString.Char8 (pack)
import Data.Char
import Data.IP
import Network
import Network.DNS
import System.IO
import System.Random         (randomRIO)
import Text.Read             (readMaybe)

-- Task IO-1.
--
-- The function 'read' is the counterpart of 'show'.
-- Its type is
--
--   read :: Read a => String -> a
--
-- It tries to parse the string as a type, depending on
-- context.
--
-- For example, the following function tries to
-- parse a string to an integer simply by restricting
-- the type signature of 'read'. Unfortunately, 'read'
-- will crash if we call it on a malformed string.
--
-- Try both a successful and an unsuccessful call to
-- 'unsafeReadInt' in GHCi.

unsafeReadInt :: String -> Int
unsafeReadInt = read

-- Task IO-2.
--
-- The function 'readMaybe', defined in the module
-- 'Text.Read', fixes this problem by returning a
-- 'Maybe' result instead. Define a safe version of
-- 'readInt' by using it.

-- |
-- >>> readInt "42"
-- Just 42
--
-- >>> readInt "I am not an Int."
-- Nothing
--
readInt :: String -> Maybe Int
readInt = readMaybe

-- Task IO-3.
--
-- Implement the IO action 'readLnMaybe' that reads a
-- line from the user, and tries to convert it into
-- a value of another readable type via 'readMaybe'.
--
-- Hint: Use 'liftM'.

readLnMaybe :: Read a => IO (Maybe a)
readLnMaybe = liftM readMaybe getLine

-- Task IO-4.
--
-- Write a program that asks the user to input a number, and after that
-- number has been provided, a second number, and then replies with the
-- sum of both numbers.
--
-- Example:
--
--   Please enter first number:
--   12
--   Please enter second number:
--   23
--   The sum of both numbers is 35.
--
-- Hint: Use 'readLn' for reading the inputs. Remember that
--
--   show :: Show a => a -> String
--
-- can be used to convert a number back into a string. Also
-- remember that strings are lists of characters.

sumTwo :: IO ()
sumTwo = do
    putStrLn "Please enter first number:"
    x <- readLn
    putStrLn "Please enter second number:"
    y <- readLn
    putStrLn $ "The sum of both numbers is " ++ show (x + y :: Int) ++ "."

-- Task IO-5.
--
-- Define a function that, given a number n and an IO action, constructs
-- a new IO action that executes the original action n times, collecting
-- the results in a list of n elements.

replicateM :: Int -> IO a -> IO [a]
replicateM !n a
    | n <= 0    = return []
    | otherwise = do
        x  <- a
        xs <- replicateM (n - 1) a
        return $ x : xs

-- Task IO-6.
--
-- Define a program that first asks the user to provide the number of
-- items to add. Then, if the user enters N, the program should ask for
-- N numbers. Once these have been entered, the program should return
-- the result.
--
-- Example:
--
--   How many numbers do you want to add?
--   3
--   Enter next number:
--   10
--   Enter next number:
--   1
--   Enter next number:
--   7
--   The sum of all numbers is 18.
--
-- Hint: Try to use suitable IO functions.

sumMany :: IO ()
sumMany = do
    putStrLn "How many numbers do you want to add?"
    n  <- readLn
    xs <- replicateM n $ putStrLn "Enter next number:" >> readLn :: IO [Int]
    putStrLn $ "The sum of all numbers is " ++ show (sum xs) ++ "."

-- Task IO-7.
--
-- Change 'sumMany' such that when asking for each number,
-- an indication of progress is given.
--
--   How many numbers do you want to add?
--   3
--   Enter number 1 of 3:
--   10
--   Enter number 2 of 3:
--   1
--   Enter number 3 of 3:
--   7
--   The sum of all numbers is 18.
--
-- Hint: You cannot use 'replicateM' for this. But perhaps
-- you can still find higher-order functions that work?

sumMany' :: IO ()
sumMany' = do
    putStrLn "How many numbers do you want to add?"
    n  <- readLn
    xs <- mapM (\i -> putStrLn ("Enter number " ++ show i ++ " of " ++ show n ++ ":") >> readLn) [1 .. n :: Int] :: IO [Int]
    putStrLn $ "The sum of all numbers is " ++ show (sum xs) ++ "."

-- Task IO-8.
--
-- Define a function akin to the Unix utility wc (word count) that for a
-- given file, returns the number of lines, of words, and of characters
-- in that file.
--
-- Note that
--
--   type FilePath = String
--
-- Hint: There are Prelude functions 'lines' and 'words' that you are
-- allowed to use.

type Words = Int
type Lines = Int
type Chars = Int

wc :: FilePath -> IO (Words, Lines, Chars)
wc fp = do
    s <- readFile fp
    return (length (lines s), length (words s), length s)

-- Task IO-9.
--
-- Define an IO action that "rolls two (6-sided) dice" and returns the
-- two results as a pair.
--
-- Use the function
--
--   randomRIO :: Random a => (a, a) -> IO a
--
-- from the module System.Random (which you will have to add to the
-- import list).
--
-- It takes a range as an input, i.e., the minimal and maximal value
-- that the random number generator should produce. The 'Int' type is
-- an instance of the 'Random' class.

twoDice :: IO (Int, Int)
twoDice = liftM2 (,) die die
  where
    die = randomRIO (1, 6)

-- Task IO-10.
--
-- The following datatype defines the abstract syntax of a language
-- of dice expressions.
--
-- For example, in an imagined concrete syntax, the string
--
--   2d8 + 4
--
-- stands for "roll two 8-sided dice and add 4". The abstract
-- representation of this expression is
--
--   2 `D` 8 `Plus` Const 4
--
-- Define an evaluator 'dice' that uses a random number generator
-- to produce a result.

data Dice =
    D Quantity Die
  | Const Int
  | Plus Dice Dice
  deriving (Show, Eq)

infix  7 `D`
infixl 6 `Plus`

type Quantity = Int
type Die      = Int

dice :: Dice -> IO Int
dice (D q d)
    | q <= 0    = return 0
    | d <= 0    = return 0
    | otherwise = sum <$> replicateM q (randomRIO (1, d))
dice (Const i)  = return i
dice (Plus x y) = liftM2 (+) (dice x) (dice y)

-- Task IO-10.
--
-- Define another function on the 'Dice' type called 'diceRange'
-- that returns the minimal and maximal random value that could
-- be produced by the given expression.
--
-- Example:
--
--   diceRange (2 `D` 8 `Plus` Const 4) == (6, 20)

diceRange :: Dice -> (Int, Int)
diceRange (D q d)
    | q <= 0    = (0, 0)
    | d <= 0    = (0, 0)
    | otherwise = (q, q * d)
diceRange (Const i)  = (i, i)
diceRange (Plus x y) =
    let (l1, u1) = diceRange x
        (l2, u2) = diceRange y
    in  (l1 + l2, u1 + u2)

-- Task IO-11.
--
-- Shuffle a deck of cards (or any other list).
--
-- Here's a standard naive algorithm:
--
-- To shuffle a list, pick a random number that is a valid index into the
-- list. Remove the element from the list and make it the first element of
-- the shuffled list. Then recurse on the shorter list. Once the empty list
-- is reached, stop.
--
-- You may need to write a function
--
--   extract :: Int -> [a] -> (a, [a]) -- possibly crashing
--
-- that finds the element contained in a list at a particular index
-- and returns it together with the rest of the list.
-- (A better data structure for this kind of operation would be a sequence.)
--
-- The following data types are just given for flavour:

data Suit = Clubs | Spades | Hearts | Diamonds
  deriving (Show, Eq, Bounded, Enum)

data CardNumber = A | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | J | Q | K
  deriving (Show, Eq, Bounded, Enum)

data Card = Card CardNumber Suit
  deriving (Show, Eq)

-- This list comprehension syntax offered by Haskell is just syntactic
-- sugar for applications of the functions 'map' and 'concat'. Do you
-- understand what it does?

allCards :: [Card]
allCards = [ Card cn s | cn <- [minBound ..], s <- [minBound ..] ]

extract :: Int -> [a] -> (a, [a])
extract _ []       = error "index out of bounds"
extract 0 (x : xs) = (x, xs)
extract i (x : xs) = let (y, ys) = extract (i - 1) xs in (y, x : ys)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    i <- randomRIO (0, length xs - 1)
    let (y, ys) = extract i xs
    zs <- shuffle ys
    return $ y : zs

-- Task IO-12.
--
-- Look at the 'Network' module from the 'network' package.
-- You can connect to a server using the function
--
--   connectTo :: HostName -> PortID -> IO Handle
--
-- With the handle, you can then use handle-based IO functions
-- from 'System.IO' such as 'hPutStrLn' and 'hGetLine'.
--
-- When finished, you should close the handle explicitly
-- using
--
--   hClose :: Handle -> IO ()
--
-- Write an IO action that connects to the Example domain at
-- example.com on port 80 and sends the
-- following two lines followed by an empty line:
--
--   GET /index.html HTTP/1.1
--   Host: example.com
--
-- Then use the helper function 'hGetLines' below to
-- read the response.

httpTest :: IO [String]
httpTest = do
    h <- connectTo "example.com" (PortNumber 80)
    hPutStrLn h "GET /index.html HTTP/1.1"
    hPutStrLn h "Host: example.com"
    hPutStrLn h ""
    xs <- hGetLines h
    hClose h
    return xs

hGetLines :: Handle -> IO [String]
hGetLines h = do
  l <- hGetLine h
  if all isSpace l
    then return []
    else do
      ls <- hGetLines h
      return (l : ls)

-- Task IO-13.
--
-- Use the function 'bracket' from module 'Control.Exception'
-- to implement a function
--
--   withConnection :: HostName -> PortID -> (Handle -> IO r) -> IO r
--
-- in the style of 'withFile'. I.e., the handle should be closed
-- automatically at the end of the continuation or when an exception
-- occurs.
--
-- Then rewrite 'httpTest' to use 'withConnection'.

withConnection :: HostName -> PortID -> (Handle -> IO r) -> IO r
withConnection h p = bracket (connectTo h p) hClose

httpTest' :: IO [String]
httpTest' = withConnection "example.com" (PortNumber 80) $ \h -> do
    hPutStrLn h "GET /index.html HTTP/1.1"
    hPutStrLn h "Host: example.com"
    hPutStrLn h ""
    hGetLines h

-- Task IO-14.
--
-- Use the function 'lookupA' to perform a DNS lookup on one
-- of the seed nodes of the bitcoin network, e.g.
--
--   seed.bitcoin.sipa.be
--
-- This function is from the Network.DNS.Lookup module from the
-- dns package. The Hackage documentation contains an example
-- of how it can be used, which involves calling other functions
-- from that package as well.
--
-- To make CI tests pass, use 'googleNameServer' (provided belowe)
-- for the 'resolvInfo' field of your 'ResolvConf'. Locally, you can
-- use the default 'ResolvConf'.

googleNameServer :: FileOrNumericHost
googleNameServer = RCHostName "8.8.8.8"

dnsTest :: IO (Either DNSError [IPv4])
dnsTest = do
    rs <- makeResolvSeed defaultResolvConf {resolvInfo = googleNameServer}
    withResolver rs $ \resolver -> lookupA resolver (pack "seed.bitcoin.sipa.be")
