# Weekly Assignments 5

### To be submitted: Friday, 4 September 2020, 12:30 MNG

Note that some tasks may deliberately ask you to look at concepts or libraries
that we have not yet discussed in detail. But if you are in doubt about the
scope of a task, by all means ask.

Please try to write high-quality code at all times!
This means in particular that you should add comments to all parts
that are not immediately obvious. Please also pay attention to
stylistic issues. The goal is always to submit code that does not
just correctly do what was asked for, but also could be committed
without further changes to an imaginary company codebase.

## W5.1 Some prisms

In this exercise, we want to explore some non-standard prisms.

### W5.1.1

Define a prism
```haskell
_Natural :: Prism' Integer Natural
```

(You can find the `Natural` type of arbitrary-precision natural numbers in module
`Numeric.Natural` in the base libraries.)
```haskell
preview _Natural 42   -- Just 42
preview _Natural (-7) -- Nothing
```

### W5.1.2

Define a function of type
```haskell
_TheOne :: Eq a => a -> Prism' a ()
```
Given an `a`, the resulting prism's focus should be the given element:
```haskell
preview (_TheOne 'x') 'x' -- Just ()
preview (_TheOne 'x') 'y' -- Nothing
review  (_TheOne 'x') ()  -- 'x'
```

### W5.1.3

Let's define the following wrapper type:
```haskell
newtype Checked a = Checked { unChecked :: a } deriving Show
```
Define a function
```haskell
_Check :: (a -> Bool) -> Prism' a (Checked a)
```
The idea is that the prism finds only elements that fulfill
the given predicate.
(This will only be a law-abiding prism if we agree to never  put an `a` into
the `Checked`-wrapper which does not satisfy the predicate.)
```
preview (_Check odd) 42         -- Nothing
preview (_Check odd) 17         -- Just (Checked {unChecked = 17})
review (_Check odd) (Checked 3) -- 3
```

## W5.2

Consider the following tree type:
```haskell
data BinTree a = Tip | Bin (BinTree a) a (BinTree a) deriving Show
```

### W5.2.1

Define three traversals
```haskell
inorder, preorder, postorder :: Traversal (BinTree a) (BinTree b) a b
```
which traverse the nodes in _inorder_ (left, value, right),
_preorder_ (value, left, right) and _postorder_ (left, right, value),
respectively.

### W5.2.2

Define two functions
```haskell
printNodes  :: Show a => Traversal' (BinTree a) a
            -> BinTree a -> IO ()

labelNodes  :: Traversal (BinTree a) (BinTree (a, Int)) a (a, Int)
            -> BinTree a -> BinTree (a, Int)
```
Given a traversal, `printNodes` should print all values stored in the tree
_in order of the traversal_, whereas `labelNodes` should label all nodes,
starting at 1, again in the order of the given traversal.

## W5.3 Delayed computations

The type constructor `Delayed` can be used to describe possibly non-terminating
computations in such a way that they remain "productive", i.e., that they produce
some amount of progress information after a finite amount of time.
```haskell
data Delayed a = Now a | Later (Delayed a)
```

We can now describe a productive infinite loop as follows:
```haskell
loop :: Delayed a
loop = Later loop
```
This is productive in the sense that we can always inspect more of the result,
and get more and more invocations of `Later`.

We can also use `Later` in other computations as a measure of cost or effort.
For example, here is a version of the factorial function in the `Delayed` type:
```haskell
factorial :: Int -> Delayed Int
factorial = go 1
  where
    go !acc n
      | n <= 0    = Now acc
      | otherwise = Later (go (n * acc) (n - 1))
```

We can extract a result from a `Delayed` computation by traversing it all the
way down until we hit a `Now`, at the risk of looping if there never is one:
```haskell
unsafeRunDelayed :: Delayed a -> a
unsafeRunDelayed (Now x)   = x
unsafeRunDelayed (Later d) = unsafeRunDelayed d
```

### Subtask 5.3.1

Define a function
```haskell
runDelayed :: Int -> Delayed a -> Maybe a
```
that extracts a result from a delayed computation if it is guarded
by at most the given number of `Later` constructors, and `Nothing` otherwise.

### Subtask 5.3.2

The type `Delayed` forms a monad, where `return` is `Now`, and `>>=` combines
the number of `Later` constructors that the left and the right argument are
guarded by.

Define the `Functor`, `Applicative`, and `Monad` instances for `Delayed`.

### Subtask 5.3.3

Assume we have
```haskell
tick :: Delayed ()
tick = Later (Now ())

psum :: [Int] -> Delayed Int
psum xs = sum <$> mapM (\ x -> tick >> return x) xs
```

Describe what `psum` does.

### Subtask 5.3.4

The type `Delayed` is actually a free monad. Define the functor `DelayedF`
such that `Free DelayedF` is isomorphic to `Delayed`, and provide the witnesses
of the isomorphism:
```haskell
fromDelayed :: Delayed a -> Free DelayedF a
toDelayed   :: Free DelayedF a -> Delayed a
```

### Subtask 5.3.5

We can also provide an instance of `Alternative`:
```haskell
instance Alternative Delayed where
  empty = loop
  (<|>) = merge

merge :: Delayed a -> Delayed a -> Delayed a
merge (Now x) _           = Now x
merge _ (Now x)           = Now x
merge (Later p) (Later q) = Later (merge p q)
```

Define a function
```haskell
firstSum :: [[Int]] -> Delayed Int
```
that performs `psum` on each of the integer lists and returns the result that
can be obtained with as few delays as possible.

Example:
```haskell
runDelayed 100 $
    firstSum [repeat 1, [1,2,3], [4,5], [6,7,8], cycle [5,6]]
```
should return `Just 9`.

### Subtask 5.3.6

Unfortunately, `firstSum` will not work on infinite (outer) lists and
```haskell
runDelayed 200 $
    firstSum $
        cycle [repeat 1, [1,2,3], [4,5], [6,7,8], cycle [5,6]]
```
will loop.

The problem is that `merge` schedules each of the alternatives in a fair way.
When using `merge` on an infinite list, all computations are evaluated one step
before the first `Later` is produced. Define
```haskell
biasedMerge :: Delayed a -> Delayed a -> Delayed a
```
that works on infinite outer lists by running earlier lists slightly sooner
than later lists. Write
```haskell
biasedFirstSum :: [[Int]] -> Delayed Int
```
which is `firstSum` in terms of `biasedMerge`. Note that `biasedFirstSum` will
not necessarily always find the shortest computation due to its biased nature,
but it should work on the infinite outer list example above and also in
```haskell
runDelayed 200 $
    biasedFirstSum $
        replicate 100 (repeat 1) ++ [[1]] ++ repeat (repeat 1)
```
to return `Just 1`.

## W5.4 Traversals (Bonus!)

Implement the following functions operating on traversals.
This is quite tricky, but if you manage it, you have really
understood traversals!
```haskell
heading    :: Traversal' s a -> Traversal' s a
tailing    :: Traversal' s a -> Traversal' s a
taking     :: Int -> Traversal' s a -> Traversal' s a
dropping   :: Int -> Traversal' s a -> Traversal' s a
filtering  :: (a -> Bool) -> Traversal' s a -> Traversal' s a
element    :: Int -> Traversal' s a -> Traversal' s a
```
In case the names are not suggestive enough -- here are the expected result
when using the various transformations:
```haskell
set (heading           each) "Addis Ababa" 'x' -- "xddis Ababa"
set (tailing           each) "Addis Ababa" 'x' -- "Axxxxxxxxxx"
set (taking 3          each) "Addis Ababa" 'x' -- "xxxis Ababa"
set (dropping 3        each) "Addis Ababa" 'x' -- "Addxxxxxxxx"
set (filtering (< 'd') each) "Addis Ababa" 'x' -- "xddisxxxxxx"
set (element 1         each) "Addis Ababa" 'x' -- "Axdis Ababa"
```

Helper functions with the following signatures might be useful:
```haskell
trans1 :: Applicative f => (a -> f a) -> (a -> Compose (State Bool) f a)
trans2 :: Applicative f => (a -> f a) -> (a -> Compose (State Int) f a)
trans3 :: Applicative f => (a -> Bool) -> (a -> f a) -> (a -> f a)
```
