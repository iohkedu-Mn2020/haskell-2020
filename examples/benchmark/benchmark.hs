import Control.Monad
import Criterion
import Criterion.Main
import Data.List      (sort)
import System.Random  (randomRIO)

main :: IO ()
main = defaultMain [ benchAlgo "sort" sort
                   , benchAlgo "qsort" quicksort
                   ]

benchAlgo :: String -> ([Int] -> [Int]) -> Benchmark
benchAlgo n s = bgroup ("sort using " ++ n) $
    map (benchLength s) [1000, 2000 .. 5000]

benchLength :: ([Int] -> [Int]) -> Int -> Benchmark
benchLength s n = bgroup ("length " ++ show n)
    [ env (return [1 .. n]) $ \xs ->
        bench "sort an already sorted list" $ nf s xs
    , env (return [n :: Int, (n - 1) .. 1]) $ \xs ->
        bench "sort a list sorted in reverse order" $ nf s xs
    , env (replicateM n $ randomRIO (1, n)) $ \xs ->
        bench "sort a list with random elements" $ nf s xs
    ]

-- Let's benchmark the standard Haskell sort function that sorts lists!

-- benchmarking in a lazy language like Haskell is tricky: We want to make sure that only the operation
-- we want to benchmark is performed during benchmarking and not other operations that have been delayed due to laziness.


-- whnf "weak head normal form" - "evaluation to the top constructor"
-- in the case of lists, which have two constructors, [], (:) it will evaluate enough to distinguish between the two.
-- Similarly, seq (or bang patterns) only evaluate to WHNF.
--
--

quicksort :: Ord a => [a] -> [a]
quicksort xs = qsort compare xs []

qsort :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
qsort _   []     r = r
qsort _   [x]    r = x:r
qsort cmp (x:xs) r = qpart cmp x xs [] [] r

-- qpart partitions and sorts the sublists
qpart :: (a -> a -> Ordering) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
qpart cmp x [] rlt rge r =
    -- rlt and rge are in reverse order and must be sorted with an
    -- anti-stable sorting
    rqsort cmp rlt (x:rqsort cmp rge r)
qpart cmp x (y:ys) rlt rge r =
    case cmp x y of
        GT -> qpart cmp x ys (y:rlt) rge r
        _  -> qpart cmp x ys rlt (y:rge) r

-- rqsort is as qsort but anti-stable, i.e. reverses equal elements
rqsort :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
rqsort _   []     r = r
rqsort _   [x]    r = x:r
rqsort cmp (x:xs) r = rqpart cmp x xs [] [] r

rqpart :: (a -> a -> Ordering) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
rqpart cmp x [] rle rgt r =
    qsort cmp rle (x:qsort cmp rgt r)
rqpart cmp x (y:ys) rle rgt r =
    case cmp y x of
        GT -> rqpart cmp x ys rle (y:rgt) r
        _  -> rqpart cmp x ys (y:rle) rgt r
