{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module W0104Spec
    ( spec
    ) where

import           Control.Monad   (replicateM)
import           Data.List       (foldl')
import qualified Data.Map        as M
import           Data.Maybe      (fromJust)
import           Prelude         hiding (lookup, null)
import           Test.Hspec
import           Test.QuickCheck

import W0104

spec :: Spec
spec = do
    describe "empty" $ do
        it "is valid"                            prop_empty_valid
    describe "genValidTrie" $
        it "generates valid tries"               $ property prop_genValidTrie_valid
    describe "insert" $ do
        it "maintains validity"                  $ property prop_insert_maintains_validity
        it "is idempotent"                       $ property prop_insert_idempotent
        it "is commutative"                      $ property prop_insert_comm
        it "increases the length by zero or one" $ property prop_insert_length
        it "reverts delete"                      $ property prop_delete_insert
    describe "delete" $ do
        it "maintains validity"                  $ property prop_delete_maintains_validity
        it "is idempotent"                       $ property prop_delete_idempotent
        it "is commutative"                      $ property prop_delete_comm
        it "decreases the length by zero or one" $ property prop_delete_length
        it "reverts insert"                      $ property prop_insert_delete
        it "gives the empty trie after applying it to all keys"
                                             $ property prop_delete_all
    describe "lookup" $ do
        it "finds inserted keys"                 $ property prop_lookup_insert
        it "does not find deleted keys"          $ property prop_lookup_delete

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (Trie a b) where

  arbitrary :: Gen (Trie a b)
  arbitrary = sized genSizedTrie

  shrink :: Trie a b -> [Trie a b]
  shrink (Fork mb m) =    (flip Fork m <$> shrink mb)
                       ++ (Fork mb     <$> shrink m)

-- | Generate a random trie with at most the given number of Fork constructors.
genSizedTrie :: (Ord a, Arbitrary a, Arbitrary b) => Int -> Gen (Trie a b)
genSizedTrie n = do
    mb <- arbitrary                           -- Chose a value for the empty key (or not).
    ts <- genSizedTries (n - 1)               -- Create subtries.
    xs <- replicateM (length ts) arbitrary    -- Keys for the subtries.
    return $ Fork mb $ M.fromList $ zip xs ts

-- | Generate a random list of tries with total number of Fork constructors
-- limited by the given argument.
genSizedTries :: (Ord a, Arbitrary a, Arbitrary b) => Int -> Gen [Trie a b]
genSizedTries n
    | n <= 0    = return []
    | otherwise = do
        t  <- genSizedTrie n
        ts <- genSizedTries $ n - countForks t
        return $ t : ts

-- | Count the number of Fork constructors in a trie.
countForks :: Trie a b -> Int
countForks (Fork _ m) = 1 + sum (countForks <$> m)

mkValid :: Trie a b -> Trie a b
mkValid (Fork mb m) = Fork mb $ M.filter (not . null) $ mkValid <$> m

-- | A QuickCheck generator for /@'valid'@/ tries.
genValidTrie :: forall a b. (Ord a, Arbitrary a, Arbitrary b) => Gen (Trie a b)
genValidTrie = mkValid <$> arbitrary

-- | Returns the keys stored in a trie.
--
-- >>> keys $ insert "foo" 42 $ insert "bar" 17 empty
-- ["bar","foo"]
keys :: Trie a b -> [[a]]
keys = map fst . toList

-- | Checks whether a given key is stored in a trie.
--
-- >>> member "foo" empty
-- False
-- >>> member "foo" $ insert "foo" 42 empty
-- True
member :: Ord a => [a] -> Trie a b -> Bool
member as t = maybe False (const True) $ lookup as t

-- | A small alphabet of just four symbols, so that key collisions become more
-- likely.
data Char' = A | B | C
    deriving (Eq, Ord)

instance Show Char' where

    show A = "A"
    show B = "B"
    show C = "C"

    showList xs = (++) $ concatMap show xs

instance Arbitrary Char' where
    arbitrary = elements [A, B, C]
    shrink = const []

type String' = [Char']

-- | Wrapper for valid tries.
newtype ValidTrie a b = ValidTrie (Trie a b)
    deriving (Show, Eq)

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (ValidTrie a b) where

    arbitrary = ValidTrie <$> genValidTrie
    shrink (ValidTrie t) = [ValidTrie (mkValid t') | t' <- shrink t]

-- | Check that the empty trie is valid.
prop_empty_valid :: Bool
prop_empty_valid = valid empty

-- | Check that the generator for valid tries does indeed generate valid tries.
prop_genValidTrie_valid :: ValidTrie Char' Int -> Bool
prop_genValidTrie_valid (ValidTrie t) = valid t

-- | Check that inserting into a valid trie does not destroy validity.
prop_insert_maintains_validity :: String' -> Int -> ValidTrie Char' Int -> Bool
prop_insert_maintains_validity s n (ValidTrie t) = valid (insert s n t)

-- | Check that inserting is idempotent.
prop_insert_idempotent :: String' -> Int -> ValidTrie Char' Int -> Property
prop_insert_idempotent s n (ValidTrie t) =
    let t' = insert s n t
    in  t' === insert s n t'

-- | Check that two inserts with different keys commute.
prop_insert_comm :: String' -> Int -> String' -> Int -> ValidTrie Char' Int -> Property
prop_insert_comm s n s' n' (ValidTrie t) =
    s /= s' ==> insert s n (insert s' n' t) === insert s' n' (insert s n t)

-- | Check that inserting into a trie changes its length by 0 or 1.
prop_insert_length :: String' -> Int -> Trie Char' Int -> Property
prop_insert_length s n t =
  let m  = member s t
      l  = length t
      l' = if m then l else l + 1
      x  = if m then "duplcate key" else "new key"
  in  label x $ length (insert s n t) === l'

-- | Wrapper for non-empty valid tries.
newtype NEValidTrie a b = NEValidTrie (Trie a b)
    deriving (Show, Eq)

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (NEValidTrie a b) where

    arbitrary = do
        ValidTrie t <- suchThat arbitrary $ \(ValidTrie t') -> not $ null t'
        return $ NEValidTrie t

    shrink = filter (\(NEValidTrie t) -> not $ null t) . shrink

-- | A non-empty trie together with an existing key.
data ValidTrieKey a b = ValidTrieKey (Trie a b) [a]
    deriving (Show, Eq)

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (ValidTrieKey a b) where

    arbitrary = do
        NEValidTrie t <- arbitrary
        as            <- elements $ keys t
        return $ ValidTrieKey t as

    shrink (ValidTrieKey t as) = [ ValidTrieKey t' as
                                 | NEValidTrie t' <- shrink (NEValidTrie t)
                                 , as `elem` keys t'
                                 ]

-- | A non-empty trie together with two existing keys.
data ValidTrieKeys a b = ValidTrieKeys (Trie a b) [a] [a]
    deriving (Show, Eq)

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (ValidTrieKeys a b) where

    arbitrary = do
        ValidTrieKey t as <- arbitrary
        as'               <- elements $ keys t
        return $ ValidTrieKeys t as as'

    shrink (ValidTrieKeys t as as') = [ ValidTrieKeys t' as as'
                                      | ValidTrieKey t' _ <- shrink (ValidTrieKey t as)
                                      , as' `elem` keys t'
                                      ]

-- | Check that deleting from a trie does not destroy validity.
prop_delete_maintains_validity :: ValidTrieKey Char' Int -> Bool
prop_delete_maintains_validity (ValidTrieKey t s) = valid (delete s t)

-- | Check that deleting from a trie is idempotent.
prop_delete_idempotent :: ValidTrieKey Char' Int -> Property
prop_delete_idempotent (ValidTrieKey t s) =
    let t' = delete s t
    in  t' === delete s t'

-- | Check that deleting from a trie is commutative.
prop_delete_comm :: ValidTrieKeys Char' Int -> Property
prop_delete_comm (ValidTrieKeys t as as') =
    let x = if as == as' then "duplicate keys" else "distinct keys"
    in  label x $ delete as (delete as' t) === delete as' (delete as t)

-- | Check that deleting from a trie decreases the length by 0 or 1.
prop_delete_length :: String' -> ValidTrie Char' Int -> Property
prop_delete_length s (ValidTrie t) =
  let m  = member s t
      x  = if m then "existing key" else "non-existing key"
      l  = length t
      l' = if m then l - 1 else l
  in  label x $ length (delete s t) === l'

-- | Check that deleting a just inserted new key does not change anything.
prop_insert_delete :: String' -> Int -> (ValidTrie Char' Int) -> Property
prop_insert_delete s n (ValidTrie t) =
  not (member s t) ==> delete s (insert s n t) === t

-- | Check that first deleting and then inserting the same
-- key-value pair does not change anything.
prop_delete_insert :: ValidTrieKey Char' Int -> Property
prop_delete_insert (ValidTrieKey t s) =
  let n = fromJust (lookup s t) :: Int
  in  insert s n (delete s t) === t

-- | Check that deleting all keys from a trie results in the empty trie.
prop_delete_all :: ValidTrie Char' Int -> Property
prop_delete_all (ValidTrie t) = foldl' (flip delete) t (keys t) === empty

-- | Check that lookup finds inserted key-value pairs.
prop_lookup_insert :: String' -> Int -> ValidTrie Char' Int -> Property
prop_lookup_insert s n (ValidTrie t) = lookup s (insert s n t) === Just n

-- | Check that lookup does not find deleted keys.
prop_lookup_delete :: ValidTrieKey Char' Int -> Property
prop_lookup_delete (ValidTrieKey t s) = lookup s (delete s t) === Nothing
