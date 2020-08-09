{-# OPTIONS_GHC -Wno-orphans #-}

module DatatypesSpec
    ( spec
    ) where

import           Prelude hiding (or, reverse)
import qualified Prelude as P
import           Test.Hspec
import           Test.QuickCheck

import           Datatypes

spec :: Spec
spec = do
    describe "implies'" $
        it "behaves like implies" $ property prop_implies'_implies
    describe "pairMaybe'" $
        it "behaves like pairMaybe" $ property prop_pair_pair'
    describe "addMaybes'" $
        it "behaves like addMaybes" $ property prop_add_add'
    describe "or" $
        it "behaves correctly" $ property prop_or
    describe "reverse" $
        it "behaves correctly" $ property prop_reverse
    describe "reverse'" $
        it "behaves correctly" $ property prop_reverse'
    describe "sameShape'" $
        it "behaves like sameShape" $ property prop_same_same'
    describe "eval" $ do
        it "satisfies prop_eval1" prop_eval1
        it "satisfies prop_eval2" prop_eval2
    describe "sub" $
        it "is compatible with eval" $ property prop_sub

arbitraryTree :: Arbitrary a => Int -> Gen (Tree a)
arbitraryTree n
    | n <= 1    = Leaf <$> arbitrary
    | otherwise = do
        k <- choose (2, n - 1)
        Node <$> arbitraryTree k <*> arbitraryTree (n - k)

instance Arbitrary a => Arbitrary (Tree a) where

    arbitrary = sized arbitraryTree

    shrink (Leaf x)   = Leaf <$> shrink x
    shrink (Node l r) = l :
                        r :
                        [Node l' r | l' <- shrink l] P.++
                        [Node l r' | r' <- shrink r]

instance Arbitrary Expr where

    arbitrary = sized $ \n ->
        if n <= 1 then Lit <$> arbitrary
                  else oneof
            [ Lit <$> arbitrary
            , let m = div n 2 in Add <$> resize m arbitrary <*> resize m arbitrary
            , Neg <$> resize (n - 1) arbitrary
            , let m = div n 3 in
                IfZero <$> resize m arbitrary <*> resize m arbitrary <*> resize m arbitrary
            ]

prop_implies'_implies :: Bool -> Bool -> Property
prop_implies'_implies x y =
    implies' x y === implies x y

prop_pair_pair' :: Maybe Int -> Maybe Int -> Property
prop_pair_pair' m n = pairMaybe' m n === pairMaybe m n

prop_add_add' :: Maybe Int -> Maybe Int -> Property
prop_add_add' m n = addMaybes' m n === addMaybes m n

prop_or :: [Bool] -> Property
prop_or bs = or bs === P.or bs

prop_reverse :: [Int] -> Property
prop_reverse xs = reverse xs === P.reverse xs

prop_reverse' :: [Int] -> Property
prop_reverse' xs = reverse' xs === P.reverse xs

prop_same_same' :: Tree Int -> Tree Int -> Property
prop_same_same' x y = sameShape' x y === sameShape x y

prop_sub :: Expr -> Expr -> Property
prop_sub x y = eval (sub x y) === eval x - eval y
