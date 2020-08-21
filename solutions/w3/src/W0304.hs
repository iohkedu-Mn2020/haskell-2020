-- | W3.4 Distinguishable Values
module W0304
    ( allBools, allEithers, allPairs, allMaybes, allFuncs
    , discriminators
    ) where

-- | All distinct values of type @'Bool'@.
allBools :: [Bool]
allBools = [True, False, undefined]

-- | All distinct values of type @'Either' 'Bool' 'Bool'@.
allEithers :: [Either Bool Bool]
allEithers = undefined : map Left allBools ++ map Right allBools

-- | All distinct values of type @('Bool', 'Bool')@.
allPairs :: [(Bool, Bool)]
allPairs = undefined : [(b1, b2) | b1 <- allBools, b2 <- allBools]

-- | All distinct values of type @'Maybe' 'Bool'@.
allMaybes :: [Maybe Bool]
allMaybes = undefined : Nothing : map Just allBools

-- | All distinct values of type @'Bool' -> 'Bool'@.
allFuncs :: [Bool -> Bool]
allFuncs = const True : const False : [\b -> if b then x else y | x <- allBools, y <- allBools]

-- | A list of functions from @('Bool', 'Bool')@ to @'Int'@ which collectively
-- can distinguish between any two distinct values of type @('Bool', 'Bool')@.
discriminators :: [(Bool, Bool) -> Int]
discriminators =
    [ \(b, c) -> let m = if b then 0 else 2
                     n = if c then 0 else 1
                 in  m + n
    , \(b, _) -> if b then 0 else 1
    , \(_, c) -> if c then 0 else 1
    , \(_, _) -> 0
    ]
