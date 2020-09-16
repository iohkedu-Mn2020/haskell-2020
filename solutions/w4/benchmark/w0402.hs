import Criterion.Main
import W0401

main :: IO ()
main = defaultMain [mkBenchmark d t |
    d <- [3, 6 .. 21],          -- testing depths 3, 6, 9, ..., 21
    t <- [First, Middle, Last]] -- testing first, middle and last index

-- |The index type.
data IndexType = First | Middle | Last
    deriving (Show, Eq)

-- |Creates a benchmark for specified depth and @'IndexType'@.
mkBenchmark :: Int -> IndexType -> Benchmark
mkBenchmark d t = env (return $ createEnv d t) $ \ ~(p, i) ->
    bench (show d ++ "-" ++ show t) $
    nf (index p) i

-- |Creates a perfect tree with the given depth.
createTree :: Int -> Perfect Int
createTree d = build d [0..]

-- |Given a depth and an @'IndexType'@, computes the corresponding index.
getIndex :: Int -> IndexType -> Int
getIndex _ First  = 0
getIndex d Middle = 2^(d - 1)
getIndex d Last   = 2^d - 1

-- |An environment consists of a perfect tree and an index.
type Env = (Perfect Int, Int)

-- |Given a depth and an @'IndexType'@, creates an environment.
createEnv :: Int -> IndexType -> Env
createEnv d t = (createTree d, getIndex d t)
