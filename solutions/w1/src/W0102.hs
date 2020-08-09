module W0102 where

-- |  The idea of merge sort is to
--    split a list into two more or less equal parts, sort them recursively,
--    and then merge two sorted lists using a dedicated merge function.
--
-- >>> mergesort [6,5,3,1,8,7,2,4]
-- [1,2,3,4,5,6,7,8]
--
mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort xs =
  let (ys, zs) = split xs
  in  merge (mergesort ys) (mergesort zs)

-- | Merges two (sorted) lists.
--
-- >>> merge "ADEX" "ABF"
-- "AABDEFX"
--
merge :: Ord a => [a] -> [a] -> [a]
merge []           ys         = ys
merge xs           []         = xs
merge xs@(x : xs') ys@(y : ys')
  | x <= y                    = x : merge xs' ys
  | otherwise                 = y : merge xs ys'

-- | Splits a list into two lists of almost equal length.
--
split :: [a] -> ([a], [a])
split []           = ([], [])
split [x]          = ([x], [])
split (x : y : zs) =
  let (xs, ys) = split zs
  in  (x : xs, y : ys)
