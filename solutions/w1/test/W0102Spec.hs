module W0102Spec
    ( spec
    ) where

import qualified Data.List as L
import           Test.Hspec
import           Test.QuickCheck

import           W0102

spec :: Spec
spec = do
    describe "split" $ do
        it "produces parts of almost equal length" $ property prop_split_length
        it "produces parts which combined contain all elements of the original list" $
            property prop_split_combine
    describe "merge" $
        it "merges" $ property prop_merge
    describe "mergesort" $
        it "sorts" $ property prop_mergesort

prop_split_length :: [Int] -> Bool
prop_split_length xs =
    let (ys, zs) = split xs
    in  abs (length ys - length zs) <= 1

prop_split_combine :: [Int] -> Property
prop_split_combine xs =
    let (ys, zs) = split xs
    in  L.sort xs === L.sort (ys ++ zs)

prop_merge :: [Int] -> [Int] -> Property
prop_merge xs ys =
    let xs' = L.sort xs
        ys' = L.sort ys
    in  merge xs' ys' === L.sort (xs ++ ys)

prop_mergesort :: [Int] -> Property
prop_mergesort xs = mergesort xs === L.sort xs
