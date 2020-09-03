module Nested where

import Data.Void

{-
data Expr =
      Var String
    | Lam String Expr
    | App Expr Expr
    deriving Show

idExpr :: Expr
idExpr = Lam "x" (Var "x") -- \x -> x  -- \x -> x

constExpr :: Expr
constExpr = Lam "x" $ Lam "_" $ Var "x" -- \x -> \_ -> x
-}

{-
data Expr a =
      Var a
    | Lam a (Expr a)
    | App (Expr a) (Expr a)
    deriving Show

idExpr :: Expr ()
idExpr = Lam () (Var ())

constExpr :: Expr Bool
constExpr = Lam True $ Lam False $ Var True
-}

data Peano a = Zero | Succ a
    deriving Show

data Expr a =
      Var a
    | Lam (Expr (Peano a))
    | App (Expr a) (Expr a)
    deriving Show

idExpr :: Expr a
idExpr = Lam (Var Zero)

constExpr :: Expr a
constExpr = Lam $ Lam $ Var (Succ Zero)

-- | Textual representation of a lambda expression.
--
-- >>> txt idExpr
-- "(Lam x0)"
--
-- >>> txt constExpr
-- "(Lam (Lam x1))"
--
txt :: Expr Void -> String
txt = txt' absurd

txt' :: (a -> Int) -> Expr a -> String
txt' f (Var a)   = "x" ++ show (f a)
txt' f (App x y) = "(" ++ txt' f x ++ " " ++ txt' f y ++ ")"
txt' f (Lam x)   = "(Lam " ++ txt' f' x ++ ")"
  where
    f' Zero     = 0
    f' (Succ a) = 1 + f a
