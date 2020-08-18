{-# LANGUAGE InstanceSigs #-}

module Rand where

import           Control.Monad
import           Data.List.NonEmpty (fromList, NonEmpty (..))
import           Data.Map           (Map)
import qualified Data.Map           as M
import           System.Random      (randomRIO)

type Prob = Double

data Rand a = End a | Coin Prob (Bool -> Rand a)

instance Functor Rand where
    fmap = liftM

instance Applicative Rand where
    pure = return
    (<*>) = ap

instance Monad Rand where

    return :: a -> Rand a
    return = End

    (>>=) :: Rand a -> (a -> Rand b) -> Rand b
    End a    >>= cont = cont a
    Coin p f >>= cont = Coin p $ \b -> f b >>= cont

coin :: Prob -> Rand Bool
coin p = Coin p End

pick :: NonEmpty a -> Rand a
pick (x :| [])       = return x
pick (x :| (y : ys)) = do
    b <- coin (1 / (2 + fromIntegral (length ys)))
    if b then return x
         else pick (y :| ys)

die :: Rand Int
die = pick (1 :| [2..6])

twoDice :: Rand Int
twoDice = (+) <$> die <*> die

runIO :: Rand a -> IO a
runIO (End a)       = return a
runIO (Coin p cont) = do
    d <- randomRIO (0, 1)
    runIO $ cont $ d < p

dist :: Ord a => Rand a -> Map a Prob
dist (End a)       = M.singleton a 1
dist (Coin p cont) =
    let dt = dist $ cont True
        df = dist $ cont False
    in  M.unionWith (+) ((* p) <$> dt) ((* (1 - p)) <$> df)

data Price = Goat | Car deriving (Show, Eq, Ord)

data Strategy = StickWithChoice | Change deriving (Show, Eq, Ord)

price :: Strategy -> Rand Price
price s = do
    carDoor     <- pick (1 :| [2, 3 :: Int])
    firstChoice <- pick (1 :| [2, 3])
    let doorsWithGoats = [d | d <- [1, 2, 3], d /= carDoor]
    hostOpens <- pick $ fromList [d | d <- doorsWithGoats, d /= firstChoice]
        -- We know this list is non-empty, because doorsWithGoats has two elements, so at least one will be left
        -- after excluding firstChoice.
    let secondChoice = case s of
            StickWithChoice -> firstChoice
            Change          -> head [d | d <- [1, 2, 3], d /= firstChoice, d /= hostOpens]
        -- We know this list has exactly one element, so head will not crash.
    return $ if secondChoice == carDoor then Car else Goat
