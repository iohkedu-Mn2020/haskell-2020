{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module DSLs2 where

-- import Control.Applicative
import System.Random

data Expr =
    Lit Int
  | Add Expr Expr
  | Rnd Int Int -- lower bound should be leq upper bound
  | Scale Int Expr -- "factor" should be non-negative
{-
  | Let String Expr Expr -- let x = e1 in e2
  | Var String
  deriving (Eq)

example :: Expr
example =
  Let "x" die (Var "x" <+> Var "x" <+> Var "x")
-}

lit :: Int -> Expr
lit = Lit

(<+>) :: Expr -> Expr -> Expr
(<+>) = Add

rnd :: Int -> Int -> Expr
rnd = Rnd

scale :: Int -> Expr -> Expr
scale = Scale

valid :: Expr -> Bool
valid (Lit _)     = True
valid (Add e1 e2) = valid e1 && valid e2
valid (Rnd l u)   = l <= u
valid (Scale f e) = f >= 0 && valid e

cost :: Expr -> Int
cost (Lit _)     = 0
cost (Add e1 e2) = cost e1 + cost e2 + 1
cost (Rnd _ _)   = 1
cost (Scale _ e) = cost e + 1

eval :: Expr -> IO Int
eval (Lit i)     = pure i
eval (Add e1 e2) = pure (+) <*> eval e1 <*> eval e2
eval (Rnd l u)   = randomRIO (l, u)
eval (Scale f e) = (f*) <$> eval e

bounds :: Expr -> (Int, Int)
bounds (Lit i)     = (i, i)
bounds (Add e1 e2) =
  case (bounds e1, bounds e2) of
    ((l1, u1), (l2, u2)) -> (l1 + l2, u1 + u2)
bounds (Rnd l u)   = (l, u)
bounds (Scale f e) =
  case bounds e of
    (l, u) -> (f * l, f * u)

die :: Expr
die = rnd 1 6








-- BREAK until 13:50



-- three random numbers, add their results
rollThreeDice :: Expr
rollThreeDice =
  die <+> die <+> die

double :: Expr -> Expr
double e = e <+> e

-- one random number, count three times
countDieResultThreeTimes :: Expr
countDieResultThreeTimes =
  lit 12 <+> scale 3 die












{-
data ExprSem d =
  ExprSem
    { lit_ :: Int -> d
    , add_ :: d -> d -> d
    , rnd_ :: Int -> Int -> d
    }

foldExpr :: ExprSem d -> Expr -> d
foldExpr (ExprSem { lit_, add_, rnd_ }) expr = go expr
  where
    go (Lit i)     = lit_ i
    go (Add e1 e2) = add_ (go e1) (go e2)
    go (Rnd l u)   = rnd_ l u

evalSem :: ExprSem (IO Int)
evalSem = ExprSem { lit_ = pure, add_ = liftA2 (+), rnd_ = \ l u -> randomRIO (l, u) }

costSem :: ExprSem Int
costSem = ExprSem { lit_ = const 0, add_ = \ c1 c2 -> c1 + c2 + 1, rnd_ = \ _ _ -> 1 }
-}






