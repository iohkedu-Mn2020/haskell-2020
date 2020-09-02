{-# LANGUAGE NamedFieldPuns #-}
module Combined where

data AExpr =
    Lit Int
  | Add AExpr AExpr
  | If BExpr AExpr AExpr

data BExpr =
    T
  | F
  | Equals AExpr AExpr
  | Not BExpr
  | Or BExpr BExpr
  | And BExpr BExpr

data ExprSem a b =
  ExprSem
    { lit_    :: Int -> a
    , add_    :: a -> a -> a
    , if_     :: b -> a -> a -> a
    , t_      :: b
    , f_      :: b
    , equals_ :: a -> a -> b
    , not_    :: b -> b
    , or_     :: b -> b -> b
    , and_    :: b -> b -> b
    }

foldAExpr :: ExprSem a b -> AExpr -> a
foldAExpr sem@(ExprSem { lit_, add_, if_ }) = go
  where
    go (Lit i)      = lit_ i
    go (Add e1 e2)  = add_ (go e1) (go e2)
    go (If b e1 e2) = if_ (foldBExpr sem b) (go e1) (go e2)

foldBExpr :: ExprSem a b -> BExpr -> b
foldBExpr sem@(ExprSem { t_, f_, equals_, not_, or_, and_ }) = go
  where
    go T              = t_
    go F              = f_
    go (Equals e1 e2) = equals_ (foldAExpr sem e1) (foldAExpr sem e2)
    go (Not b)        = not_ (go b)
    go (Or b1 b2)     = or_ (go b1) (go b2)
    go (And b1 b2)    = and_ (go b1) (go b2)

aeval :: AExpr -> Int
aeval (Lit i)      = i
aeval (Add e1 e2)  = aeval e1 + aeval e2
aeval (If b e1 e2) = if beval b then aeval e1 else aeval e2

beval :: BExpr -> Bool
beval T              = True
beval F              = False
beval (Equals e1 e2) = aeval e1 == aeval e2
beval (Not b)        = not (beval b)
beval (And b1 b2)    = beval b1 && beval b2
beval (Or b1 b2)     = beval b1 || beval b2

evalSem :: ExprSem Int Bool
evalSem =
  ExprSem
    { lit_    = id
    , add_    = (+)
    , if_     = \ b t e -> if b then t else e
    , t_      = True
    , f_      = False
    , equals_ = (==)
    , not_    = not
    , and_    = (&&)
    , or_     = (||)
    }
