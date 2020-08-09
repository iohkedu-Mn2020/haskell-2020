module W0107 where

import           Control.Monad       (ap, liftM)
import           Control.Monad.State (State, evalState)
import qualified Control.Monad.State as S
import           Text.Read           (readMaybe)

data GP a = End a
          | Get (Int -> GP a)
          | Put Int (GP a)

echo :: GP a
echo = Get (\n -> Put n echo)

run :: GP a -> IO a
run (End x)   = return x
run (Get f)   = readInt >>= run . f
 where
  readInt :: IO Int
  readInt = do
    putStr "? "
    s <- getLine
    maybe readInt return $ readMaybe s
run (Put n m) = print n >> run m

add :: GP ()
add =
  Get $ \n ->
  Get $ \m ->
  Put (m + n) $
  End ()

accum :: GP Int
accum = go 0
 where
  go :: Int -> GP Int
  go t =
    Get $ \n ->
    if n == 0
      then End t
      else let t' = t + n
           in  Put t' (go t')

simulate :: GP a -> [Int] -> (a, [Int])
simulate (End x)   _        = (x, [])
simulate (Get _)   []       = error "insufficient input"
simulate (Get f)   (n : ns) = simulate (f n) ns
simulate (Put y m) ns       = let (z, ys) = simulate m ns
                              in  (z, y : ys)

simulateS :: GP a -> State [Int] (a, [Int])
simulateS (End x)   = return (x, [])
simulateS (Get f)   = do
  ys <- S.get
  case ys of
    []       -> error "empty list"
    (x : xs) -> do
      S.put xs
      simulateS $ f x
simulateS (Put y m) = do
  ~(z, ys) <- simulateS m
  return (z, y : ys)

simulate' :: GP a -> [Int] -> (a, [Int])
simulate' m xs = evalState (simulateS m) xs

instance Functor GP where
    fmap = liftM

instance Applicative GP where
    pure  = return
    (<*>) = ap

instance Monad GP where
    return = End

    End x   >>= continuation = continuation x
    Get f   >>= continuation = Get $ \x -> f x >>= continuation
    Put x m >>= continuation = Put x $ m >>= continuation

get :: GP Int
get = Get End

put :: Int -> GP ()
put x = Put x $ End ()

echo' :: GP a
echo' = get >>= put >> echo'

add' :: GP ()
add' = do
  n <- get
  m <- get
  put $ m + n

accum' :: GP Int
accum' = go 0
 where
  go :: Int -> GP Int
  go t = do
    n <- get
    if n == 0
      then return t
      else do
        let t' = t + n
        put t'
        go t'
