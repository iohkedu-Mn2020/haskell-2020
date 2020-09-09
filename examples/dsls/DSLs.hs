{-# LANGUAGE BangPatterns #-}
module DSLs where

{-
data Expr -- abstract

lit :: Int -> Expr
lit = undefined

(<+>) :: Expr -> Expr -> Expr
(<+>) = undefined
-}

{-
-- Count the number of <+> occurrences:
type Expr = Int

lit :: Int -> Expr
lit _ = 0

(<+>) :: Expr -> Expr -> Expr
-- (<+>) e1 e2 = e1 + e2 + 1
e1 <+> e2 = e1 + e2 + 1
-}

{-
-- Evaluate the expression:
type Expr = Int

lit :: Int -> Expr
lit = id

(<+>) :: Expr -> Expr -> Expr
(<+>) = (+)
-}

{-
-- String representation of an expression:
type Expr = String

lit :: Int -> Expr
lit = show

(<+>) :: Expr -> Expr -> Expr
e1 <+> e2 = "(" ++ e1 ++ " + " ++ e2 ++ ")"
-}

{-
type Expr = (Int, Int) -- cost, value

lit :: Int -> Expr
lit i = (0, i)

(<+>) :: Expr -> Expr -> Expr
(c1, v1) <+> (c2, v2) = (c1 + c2 + 1, v1 + v2)
-}

data Expr =
    Lit        Int
  | Add        Expr Expr
  deriving (Eq)

lit :: Int -> Expr
lit = Lit

(<+>) :: Expr -> Expr -> Expr
(<+>) = Add

cost :: Expr -> Int
cost (Lit _)     = 0
cost (Add e1 e2) = cost e1 + cost e2 + 1

eval :: Expr -> Int
eval (Lit i)     = i
eval (Add e1 e2) = eval e1 + eval e2

costAndEval :: Expr -> (Int, Int)
costAndEval e = (cost e, eval e)

simplify :: Expr -> Expr
simplify (Lit i)     = Lit i
simplify (Add e1 e2) =
  case (simplify e1, simplify e2) of
    (Lit 0, e2') -> e2'
    (e1', Lit 0) -> e1'
    (e1', e2')   -> Add e1' e2'

txt :: Expr -> String
txt (Lit i)     = show i
txt (Add e1 e2) = txt e1 ++ " + " ++ txt e2

-- Lit :: Int -> Expr
-- Add :: Expr -> Expr -> Expr

type ExprSem d = (Int -> d, d -> d -> d)

{-
data ExprSem' d =
  ExprSem'
    { litSem :: Int -> d
    , addSem :: d -> d -> d
    , ...
    }
-}

foldExpr :: ExprSem d -> Expr -> d
foldExpr (lit_, add_) expr = go expr
  where
    go (Lit i)       = lit_ i
    go (Add !e1 !e2) = add_ (go e1) (go e2)

q1Sem :: ExprSem Expr
q1Sem = (Lit, Add)

q2Sem :: ExprSem Bool
q2Sem = ((>0), (&&))

data StrictPair a b = StrictPair !a !b
  deriving Show

pairSem :: ExprSem a -> ExprSem b -> ExprSem (StrictPair a b)
pairSem (lit1_, add1_) (lit2_, add2_) =
  ( \ i -> StrictPair (lit1_ i) (lit2_ i)
  , \ (StrictPair a1 b1) (StrictPair a2 b2) -> StrictPair (add1_ a1 a2) (add2_ b1 b2)
  )

evalSem :: ExprSem Int
evalSem = (id, (+))

costSem :: ExprSem Int
costSem = (const 0, \ c1 c2 -> c1 + c2 + 1)

txtSem :: ExprSem String
txtSem = (show, \ t1 t2 -> t1 ++ " + " ++ t2)

simplifySem :: ExprSem Expr
simplifySem = (Lit, simplifyAdd)
  where
    simplifyAdd (Lit 0) e2' = e2'
    simplifyAdd e1' (Lit 0) = e1'
    simplifyAdd e1' e2'     = Add e1' e2'

{-
foldExpr lit_ add_ (Lit i) = lit_ i
foldExpr lit_ add_ (Add e1 e2) = add_ (foldExpr lit_ add_ e1) (foldExpr lit_ add_ e2)
-}

prop_simplifyPreservesValue :: Expr -> Bool
prop_simplifyPreservesValue e =
  eval e == eval (simplify e)

prop_simplifyReducesCost :: Expr -> Bool
prop_simplifyReducesCost e =
  cost e >= cost (simplify e)

prop_simplifyIdempotent :: Expr -> Bool
prop_simplifyIdempotent e =
  simplify e == simplify (simplify e)

example1 :: Expr
example1 = lit 4 <+> lit 0 <+> lit 12

example2 :: Expr
example2 = lit 33

example3 :: Expr
example3 = lit 0 <+> lit 0 <+> (lit 0 <+> lit 0 <+> lit 0)

example3' :: Expr
example3' = lit 0

double :: Expr -> Expr
double x = x <+> x

large :: Expr
large =
  double $ double $ double $ double $
  double $ double $ double $ double $ double $ double $ double $ double $ double $ double $
  double $ double $ double $ double $ double $ double $ double $ double $ double $ double $
  lit 1

large' :: Expr
large' =
  simplify large

main :: IO ()
main =
  print (foldExpr (pairSem costSem evalSem) large')

-- example4 :: Expr
-- example4 = lit 'x'
--
-- example5 :: Expr
-- example5 = lit 3 <*> lit 5



--   example1
-- =
--   (lit 4 <+> lit 0) <+> lit 12
-- = ^^^^^^^^^^^^^^^^^     ^^^^^^
--          e1               e2
--
--   ((lit 4 <+> lit 0) + lit 12) + 1
-- =   ^^^^^     ^^^^^
--       e1        e2
--
--   (((lit 4 + lit 0) + 1) + lit 12) + 1
-- =
--   (((0 + lit 0) + 1) + lit 12) + 1
-- =
--   (((0 + 0) + 1) + lit 12) + 1
-- =
--   ((0 + 1) + lit 12) + 1
-- =
--   (1 + lit 12) + 1
-- =
--   (1 + 0) + 1
-- =
--   1 + 1
-- =
--   2




















