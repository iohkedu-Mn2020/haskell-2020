{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module HigherOrderSpec
    ( spec
    ) where

import qualified Control.Monad.State as S
import           Data.List           (sort, nub)
import           Data.Maybe          (fromMaybe)
import qualified Prelude             as P
import           Prelude             hiding (product, reverse, take)
import           Test.Hspec
import           Test.QuickCheck

import           HigherOrder

spec :: Spec
spec = do
    describe "product" $
        it "behaves like Prelude.product" $ property $ prop_product product
    describe "product'" $
        it "behaves like Prelude.product" $ property $ prop_product product'
    describe "reverse" $
        it "behaves like Prelude.reverse" $ property prop_reverse
    describe "mapBinTree" $
        it "maps id to id" $ property prop_mapBinTree
    describe "isBST" $ do
        it "works for random trees" $ property prop_isBST_random
        it "works for search trees" $ property prop_isBST_search
    describe "search" $ do
        it "finds an existing value" $ property prop_search_existing
        it "does not find a non-existing value" $ property prop_search_non_existing
    describe "insert" $ do
        it "preserves the BST property" $ property prop_insert_BST
        it "is idempotent" $ property prop_insert_idempotent
    describe "sortDescending" $
        it "works" $ property prop_sortDescending
    describe "fromListBST" $ do
        it "creates a BST" $ property prop_fromListBST_isBST
        it "can be used to sort" $ property prop_fromListBST_sort
    describe "labelTree" $ do
        it "works" $ property prop_labelTree
    describe "take" $
        it "behaves like Prelude.take" $ property prop_take
    describe "myFoldl" $
        it "works to implement reverse" $ property prop_myFoldl_reverse
    describe "foldrBinTree" $
        it "is compatible with toList" $ property prop_foldrBinTree_toList

prop_product :: ([Integer] -> Integer) -> [Integer] -> Property
prop_product p xs = p xs === P.product xs

prop_reverse :: [Int] -> Property
prop_reverse xs = reverse xs === P.reverse xs

instance Arbitrary a => Arbitrary (BinTree a) where

    arbitrary = sized $ \case
        s
            | s <= 1    -> return Empty
            | otherwise -> do
                sl <- choose (1, s - 1)
                let sr = s - sl
                Bin <$> resize sl arbitrary
                    <*> arbitrary
                    <*> resize sr arbitrary

    shrink Empty = []
    shrink (Bin l x r) =
        l :
        r :
        [Bin l' x r | l' <- shrink l] ++
        [Bin l x' r | x' <- shrink x] ++
        [Bin l x r' | r' <- shrink r]

prop_mapBinTree :: BinTree Int -> Property
prop_mapBinTree t = mapBinTree id t === t

toList :: BinTree a -> [a]
toList Empty       = []
toList (Bin l x r) = toList l ++ [x] ++ toList r

prop_isBST_random :: BinTree Int -> Property
prop_isBST_random t = isBST t === let xs = toList t in xs == sort (nub xs)

minBST :: BST a -> Maybe a
minBST Empty = Nothing
minBST (Bin l x _) = case minBST l of
    Nothing -> Just x
    Just y  -> Just y

maxBST :: BST a -> Maybe a
maxBST Empty = Nothing
maxBST (Bin _ x r) = case maxBST r of
    Nothing -> Just x
    Just y  -> Just y

newtype BST' = BST' (BST Integer)
    deriving newtype Show

fmapBinTree :: (a -> b) -> BinTree a -> BinTree b
fmapBinTree _ Empty       = Empty
fmapBinTree f (Bin l x r) = Bin (fmapBinTree f l) (f x) (fmapBinTree f r)

instance Arbitrary BST' where

    arbitrary = sized $ \case
        s
            | s <= 1    -> return (BST' Empty)
            | otherwise -> do
                sl <- choose (1, s - 1)
                let sr = s - sl
                x <- arbitrary
                BST' l' <- resize sl arbitrary
                BST' r' <- resize sr arbitrary
                let lMax   = fromMaybe (x - 1) $ maxBST l'
                    rMin   = fromMaybe (x + 1) $ minBST r'
                    lDelta = max 0 $ lMax - x + 1
                    rDelta = max 0 $ x - rMin + 1
                    l      = fmapBinTree (+ (- lDelta)) l'
                    r      = fmapBinTree (+ rDelta)     r'
                return $ BST' $ Bin l x r

    shrink (BST' Empty) = []
    shrink (BST' (Bin l x r)) =
        BST' l :
        BST' r :
        [BST' (Bin l' x r) | BST' l' <- shrink (BST' l)] ++
        [BST' (Bin l x r') | BST' r' <- shrink (BST' r)]

prop_isBST_search :: BST' -> Bool
prop_isBST_search (BST' t) = isBST t

prop_search_existing :: Integer -> BST' -> Bool
prop_search_existing x (BST' t) = search x $ insert x t

delete :: Ord a => a -> BST a -> BST a
delete _ Empty = Empty
delete x (Bin l y r)
    | x < y     = Bin (delete x l) y r
    | x > y     = Bin l y (delete x r)
    | otherwise = case maxBST l of
        Nothing -> r
        Just z  -> Bin (delete z l) z r

prop_search_non_existing :: Integer -> BST' -> Bool
prop_search_non_existing x (BST' t) = not $ search x $ delete x t

prop_insert_BST :: Integer -> BST' -> Bool
prop_insert_BST x (BST' t) = isBST $ insert x t

prop_insert_idempotent :: Integer -> BST' -> Property
prop_insert_idempotent x (BST' t) = let t' = insert x t in insert x t' === t'

prop_sortDescending :: [Int] -> Property
prop_sortDescending xs = sortDescending xs === reverse (sort xs)

prop_fromListBST_isBST :: [Int] -> Bool
prop_fromListBST_isBST xs = isBST (fromListBST xs)

prop_fromListBST_sort :: [Int] -> Property
prop_fromListBST_sort xs = toList (fromListBST xs) === nub (sort xs)

traverseBinTree :: Applicative f => (a -> f b) -> BinTree a -> f (BinTree b)
traverseBinTree _ Empty       = pure Empty
traverseBinTree f (Bin l x r) = Bin <$> traverseBinTree f l <*> f x <*> traverseBinTree f r

prop_labelTree :: BinTree Char -> Property
prop_labelTree t = labelTree t === S.evalState (traverseBinTree f t) 1
  where
    f c = do
        l <- S.get
        S.put $ succ l
        return (c, l)

prop_take :: Int -> [Int] -> Property
prop_take n xs = label l $ take n xs === P.take n xs
  where
    l :: String
    l | n < 0          = "negative"
      | n == 0         = "zero"
      | n >= length xs = "too many"
      | otherwise      = "in range"

prop_myFoldl_reverse :: [Int] -> Property
prop_myFoldl_reverse xs = myFoldl (flip (:)) [] xs === P.reverse xs

prop_foldrBinTree_toList :: BinTree Int -> Property
prop_foldrBinTree_toList t = foldrBinTree (:) [] t === toList t
