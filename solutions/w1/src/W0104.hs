{-# LANGUAGE InstanceSigs #-}

module W0104 where

import           Data.Map        (Map)
import qualified Data.Map        as M
import           Prelude         hiding (lookup, null)

-- |A @'Trie' a b@ is a map with keys of type @[a]@ and values of type @b@.
data Trie a b = Fork (Maybe b) (Map a (Trie a b))
  deriving (Show, Eq)

instance Functor (Trie a) where

  fmap :: (b -> c) -> Trie a b -> Trie a c
  fmap f (Fork mb m) = Fork (f <$> mb) (fmap f <$> m)

instance Foldable (Trie a) where

  foldr :: (b -> r -> r) -> r -> Trie a b -> r
  foldr f c t = foldr f c $ map snd $ toList t

-- | Converts a trie to a list of key-value pairs.
--
-- >>> toList $ insert "foo" 42 $ insert "bar" 17 empty
-- [("bar",17),("foo",42)]
toList :: Trie a b -> [([a], b)]
toList (Fork mb m) =
  let xs = [(a : as, b) | (a, t) <- M.toList m, (as, b) <- toList t]
  in  case mb of
        Nothing -> xs
        Just b  -> ([], b) : xs

-- | The empty trie.
--
-- >>> length empty
-- 0
--
empty  :: Trie a b
empty = Fork Nothing M.empty

-- | Checks whether a trie is empty.
-- This works for /all/ tries, not just /valid/ ones
-- (and is used in the definition of @`valid`@).
--
-- >>> null empty
-- True
-- >>> null (insert "IOHK" True empty)
-- False
--
null   :: Trie a b -> Bool -- checks if a trie is empty
null (Fork (Just _) _) = False
null (Fork Nothing m)  = all null m

-- | Checks whether a trie is valid
-- (all tries produced by the public API of this module are valid).
--
valid :: Trie a b -> Bool
valid (Fork _ m) = all (\t -> valid t && not (null t)) m

-- | Inserts a value for a key into a trie.
--
insert :: Ord a
       => [a]      -- ^ the key
       -> b        -- ^ the value
       -> Trie a b -- ^ the trie
       -> Trie a b -- ^ a trie with the given value associated to the given key
insert []       b (Fork _ m)  = Fork (Just b) m -- If the key list is empty, we have found the right spot to put the value.
insert (a : as) b (Fork mb m) =                 -- If not...
  let t'  = M.findWithDefault empty a m         -- ...we find the correct sub-trie (or take the empty one),
      t'' = insert as b t'                      -- insert the value recursively
      m'  = M.insert a t'' m                    -- and put the updated sub-trie back.
  in  Fork mb m'

-- | Looks up a key in a trie. Returns @'Just'@ the value or @'Nothing'@.
--
-- >>> lookup "foo" empty
-- Nothing
-- >>> lookup "foo" (insert "foo" 42 empty)
-- Just 42
--
lookup :: Ord a
       => [a]      -- ^ the key
       -> Trie a b -- ^ the trie
       -> Maybe b  -- ^ @'Just' b@ if @b@ is associated to the given key; otherwise @'Nothing'@
lookup []       (Fork mb _) = mb
lookup (a : as) (Fork _  m) = do
  t <- M.lookup a m
  lookup as t

-- | Deletes a key (and its associated value) from a trie.
--
-- >>> lookup "foo" $ delete "foo" $ insert "foo" 42 empty
-- Nothing
--
delete :: Ord a
       => [a]      -- ^ the key to delete
       -> Trie a b -- ^ the trie
       -> Trie a b -- ^ the trie with the given key (and its associated value) deleted
delete [] (Fork _ m)          = Fork Nothing m        -- If the key is the empty list, simply delete the value.
delete (a : as) t@(Fork mb m) = case M.lookup a m of
  Nothing -> t                                        -- If the head of the key is not in the dictionary, there's
                                                      -- nothing to delete.
  Just t' ->
    let t'' = delete as t'                            -- Delete recursively.
        m'  = if null t''
          then M.delete a m                           -- If the resulting sub-trie is empty, we remove it from the
                                                      -- dictionary to keep the trie valid.
          else M.insert a t'' m                       -- Otherwise, we update the sub-trie.
    in  Fork mb m'
