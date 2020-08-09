module W0101Spec
    ( spec
    ) where

import qualified Data.List as L
import           Test.Hspec
import           Test.QuickCheck

import           W0101

spec :: Spec
spec = do
    describe "permutations" $ do
        it "generates all permutations" $ property prop_permutations

prop_permutations :: [Int] -> Property
prop_permutations xs =
    let ys = take 6 xs -- only test lists up to length six
    in  L.sort (permutations ys) === L.sort (L.permutations ys)
