{-# OPTIONS_GHC -Wno-orphans #-}

module W0103Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck

import W0103

spec :: Spec
spec = do
    describe "splitleft'" $ do
        it "behaves like splitleft" $ property prop_splitleft'

prop_splitleft' :: Tree Int -> Property
prop_splitleft' t = splitleft' t === splitleft t

instance Arbitrary a => Arbitrary (Tree a) where

    arbitrary = sized $ \s -> if s <= 1
        then Leaf <$> arbitrary
        else oneof [ Leaf <$> arbitrary
                   , do
                        sl <- choose (1, s - 1)
                        let sr = s - sl
                        Node <$> resize sl arbitrary <*> resize sr arbitrary
                   ]

    shrink (Leaf x)   = Leaf <$> shrink x
    shrink (Node l r) = l :
                        r :
                        [Node l' r | l' <- shrink l] ++
                        [Node l r' | r' <- shrink r]
