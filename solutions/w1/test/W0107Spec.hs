module W0107Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck

import W0107

spec :: Spec
spec = do
    describe "add" $
        it "works as expected" $ property prop_add
    describe "accum" $
        it "works as expected" $ property prop_accum

prop_add :: Int -> Int -> Property
prop_add m n = simulate add [m, n] === ((), [m + n])

prop_accum :: [Int] -> Property
prop_accum xs =
    let xs' = filter (/= 0) xs
    in  simulate accum (xs' ++ [0]) === (sum xs', tail $ scanl (+) 0 xs')
