{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Prop where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Data.Set (Set)
import qualified Data.Set as Set
import Test.QuickCheck

data Prop =
    Var String
  | T
  | F
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  deriving (Show, Eq)

instance Arbitrary Prop where

    arbitrary = oneof
        [ Var <$> arbitrary
        , pure T
        , pure F
        , Not <$> arbitrary
        , And <$> arbitrary <*> arbitrary
        , Or  <$> arbitrary <*> arbitrary
        ]


type Environment = String -> Bool

cost :: Prop -> Int
cost (Var _)     = 100
cost T           = 0
cost F           = 0
cost (Not p)     = 1 + cost p
cost (And p1 p2) = 1 + cost p1 + cost p2
cost (Or p1 p2)  = 1 + cost p1 + cost p2

eval :: Prop -> Environment -> Bool
eval (Var x)     env = env x
eval T           _   = True
eval F           _   = False
eval (Not p)     env = not (eval p env)
eval (And p1 p2) env = eval p1 env && eval p2 env
eval (Or p1 p2)  env = eval p1 env || eval p2 env

newtype BEval a = BEval (ReaderT Environment (StateT Int Maybe) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadReader Environment
    , MonadState Int
    , MonadError ()
    )

pay :: Int -> BEval ()
pay amount = do
  budget <- get
  if budget < amount
    then throwError ()
    else put (budget - amount)

lookupInEnv :: String -> BEval Bool
lookupInEnv x = do
  env <- ask
  return (env x)

runBEval :: BEval a -> Environment -> Int -> Maybe a
runBEval (BEval m) env budget =
  evalStateT (runReaderT m env) budget

beval :: Prop -> BEval Bool
beval (Var x) = do
  pay 100
  lookupInEnv x
beval T       = return True
beval F       = return False
beval (Not p) = do
  pay 1
  p' <- beval p
  return (not p')
beval (And p1 p2) = do
  pay 1
  (&&) <$> beval p1 <*> beval p2
beval (Or p1 p2) = do
  pay 1
  (||) <$> beval p1 <*> beval p2

-- pure x <*> y  ==  x <$> y
-- pure x <*> y <*> z  == x <$> y <*> z  ==  do { y' <- y; z' <- z; return (x y' z') }

empty :: Environment
empty = const False

extend :: Environment -> String -> Bool -> Environment
extend env x b = \ y -> if y == x then b else env y

vars :: Prop -> Set String
vars (Var x)     = Set.singleton x
vars T           = Set.empty
vars F           = Set.empty
vars (Not p)     = vars p
vars (And p1 p2) = vars p1 `Set.union` vars p2
vars (Or p1 p2)  = vars p1 `Set.union` vars p2

assignments :: [String] -> [Environment]
assignments []       = [empty]
assignments (x : xs) =
  -- let
  --   envs = assignments xs
  -- in
  --   concat (map (\ env -> [extend env x False, extend env x True]) envs)

  do
    env <- assignments xs
    [extend env x False, extend env x True]

tautology :: Prop -> Bool
tautology p =
  let
    varsInProp :: [String]
    varsInProp = Set.toList (vars p)

    relevantAssignments :: [Environment]
    relevantAssignments = assignments varsInProp
  in
    and (map (\env -> eval p env) relevantAssignments)
