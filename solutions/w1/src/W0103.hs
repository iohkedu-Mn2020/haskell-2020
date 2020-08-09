{-# LANGUAGE ScopedTypeVariables #-}

module W0103 where

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Eq)

splitleft :: Tree a -> (a, Maybe (Tree a))
splitleft (Leaf a  ) = (a, Nothing)
splitleft (Node l r) = case splitleft l of
  (a, Nothing) -> (a, Just r)
  (a, Just l') -> (a, Just (Node l' r))

-- | A tail-recursive version of @'splitleft'@.
-- @`splitLeft'`@ just calls the helper function
-- @`splitLeftCont`@ with @`id`@ as continuation.
splitleft' :: Tree a -> (a, Maybe (Tree a))
splitleft' t = splitleftCont t id

splitleftCont :: forall a b.
                 Tree a                -- ^the tree
              -> (Maybe (Tree a) -> b) -- ^the continuation
              -> (a, b)                -- ^returns the left-most element and the result
                                       -- of applying the continuation to the (optional) rest of the tree.
splitleftCont (Leaf a)   cont = (a, cont Nothing)
splitleftCont (Node l r) cont =
  let f :: Maybe (Tree a) -> Tree a
      f Nothing   = r
      f (Just l') = Node l' r
  in  splitleftCont l (cont . Just . f)
