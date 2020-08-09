{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module TablesSpec
    ( spec
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Prelude         hiding (lookup)
import           Test.Hspec
import           Test.QuickCheck

import           Tables

spec :: Spec
spec = do
    describe "insert" $
        it "is compatible with M.insert" $ property prop_insert
    describe "delete" $
        it "is compatible with M.delete" $ property prop_delete
    describe "lookup" $
        it "is compatible with M.lookup" $ property prop_lookup
    describe "mapValues" $
        it "is compatible with fmap on Map" $ property prop_mapValues
    describe "mapKeys" $
        it "is compatible with M.mapKeys for an injective function" $ property prop_mapKeys
    describe "alter" $
        it "is compatible with M.alter" $ property prop_alter

deriving newtype instance (Eq k, Eq v) => Eq (Table k v)

instance (Arbitrary k, Arbitrary v) => Arbitrary (Table k v) where
    arbitrary = Table <$> arbitrary

fromTable :: Ord k => Table k v -> Map k v
fromTable (Table xs) = foldr (uncurry M.insert) M.empty xs

prop_insert :: Int -> Char -> Table Int Char -> Property
prop_insert n c t = fromTable (insert n c t) === M.insert n c (fromTable t)

prop_delete :: Int -> Table Int Char -> Property
prop_delete n t = fromTable (delete n t) === M.delete n (fromTable t)

prop_lookup :: Int -> Table Int Bool -> Property
prop_lookup n t = lookup n t === M.lookup n (fromTable t)

prop_mapValues :: Fun Char Bool -> Table Int Char -> Property
prop_mapValues f t = fromTable (mapValues (applyFun f) t) === (applyFun f <$> fromTable t)

prop_mapKeys :: Table Int Char -> Property
prop_mapKeys t = fromTable (mapKeys succ t) === M.mapKeys succ (fromTable t)

prop_alter :: Fun (Maybe Char) (Maybe Char) -> Int -> Table Int Char -> Property
prop_alter f n t = fromTable (alter (applyFun f) n t) === M.alter (applyFun f) n (fromTable t)
