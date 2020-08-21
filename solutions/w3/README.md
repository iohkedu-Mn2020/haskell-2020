# Weekly Assignments 3

### To be submitted: Friday, 21 August 2020, 12:30

Note that some tasks may deliberately ask you to look at concepts or libraries
that we have not yet discussed in detail. But if you are in doubt about the
scope of a task, by all means ask.

Please try to write high-quality code at all times!
This means in particular that you should add comments to all parts
that are not immediately obvious. Please also pay attention to
stylistic issues. The goal is always to submit code that does not
just correctly do what was asked for, but also could be committed
without further changes to an imaginary company codebase.

## W3.1 A simple stack language

Cryptocurrencies typically come with scripting languages. These
scripting languages vary in power quite a bit. Ethereum and Cardano have a
Turing complete and complicated scripting language that is supposed
to enable sophisticated smart contracts. Bitcoin still has a
scripting language, but it is much more restricted. Bitcoin's
language is actually a stack-based language. In this exercise,
we are going to implement an extremely simple stack-based language
and an evaluator for that language.

The abstract syntax of the language is given by
```haskell
data Instructions =
    Push Int Instructions
  | Add Instructions
  | Mul Instructions
  | Dup Instructions
  | Swap Instructions
  | Neg Instructions
  | Pop Instructions
  | Over Instructions
  | IfZero Instructions Instructions
  | Loop (Instructions -> Instructions)
  | Halt
```

The description of each of the instructions is as follows:

- `Push` pushes the given integer on the top of the stack,
- `Add` removes the top two elements from the stack and pushes their sum,
- `Mul` removes the top two elements from the stack and pushes their product,
- `Dup` duplicates the top element of the stack,
- `Swap` swaps the top two elements of the stack,
- `Neg` negates the top element of the stack,
- `Pop` removes the top element from the stack,
- `Over` pushes a copy of the element just beyond the top on top of the
  stack (e.g., if the top element is `1` and the next element is `2`,
  then the new stack has top element `2`, followed by `1` and `2`)
- `IfZero` removes the top element of the stack, and if that
  element is `0`, executes the first set of instructions, otherwise
  it executes the second set of instructions,
- `Loop` executes the function, passing itself as an argument
- `Halt` stops execution.

The goal is to implement a function
```haskell
run :: Instructions -> Maybe [Int]
```
that runs a set of instructions on an initially empty
stack and returns the final stack.
It should return `Nothing` if at any point in time, there are
not sufficiently many elements on the stack.

You may want to use a suitable monad, but this is not a
requirement.

Here is the factorial function as an example program:
```
fact5 :: Instructions
fact5 =
  Push 5 $
  Push 1 $
  Swap $
  Loop $ \ loop ->
  Dup $
  IfZero (Pop $ Halt) $
  Swap $
  Over $
  Mul $
  Swap $
  Push 1 $
  Neg $
  Add $
  loop
```
The very first `Push` is the argument. So this program should
evaluate to the stack containing just `120`.

## W3.2 A parser for stack programs

Define a Megaparsec parser so that you have a concrete syntax for the
abstract syntax given in exercise W3.1.

The goal is to be able to write the example program as follows:
```
{
  push 5;
  push 1;
  swap;
  loop {
    dup;
    ifzero {
      pop;
      halt
    } {
      swap;
      over;
      mul;
      swap;
      push 1;
      neg;
      add;
      ret
    }
  }
}
```

The grammar is
```
block  -> "{" instrs "}"
instrs -> simple ";" instrs | ctrl
simple -> "push " int | "add" | "mul" | "dup" | "swap" | "neg"|
          "pop" | "over"
ctrl   -> "ifzero" block block | "loop" block | "halt" | "ret"
int denotes an optionally signed integer number
```

A `ret` indicates that we want to return to the beginning of the
loop. If a `ret` occurs outside of a loop, it is interpreted as `halt`.

Layout is not important. Arbitrary whitespace is allowed between
any two tokens. As tokens, we consider all the literal strings
that occur in the grammar, i.e. `"{"`, `"}"`, `";"`, `"push"`,
`"add"`,..., `"ret"`, as well as `int`'s.

Try to find a disciplined handling of whitespace (i.e., either
write a separate lexer, or find a way to isolate whitespace handling
so that it does not occur all over the place).

Write a function that reads a source file from disk, parses it,
and if parsing is successful, runs it through the stack evaluator
from the previous assignment.

## W3.3 Testing binary trees

In this task, we want to practice writing `QuickCheck` generators
at the example of our standard binary tree type
```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq, Functor)
```
If we are not careful, the randomly generated trees will be far
too large, so we have to make sensible use of the `sized`
function.

### Subtask 3.3.1

Write a function
```haskell
genTree :: Arbitrary a => Int -> Gen (Tree a)
```
that generates trees whose number of leaves equals
the given argument. There are no trees with less than one
leaf, so it is okay if your function crashes for non-positive
arguments.

### Subtask 3.3.2

Use `genTree` from 3.4.1 to implement
```haskell
instance Arbitrary a => Arbitrary (Tree a) where
...
```
with the help of `sized`: For a given size,
generate a tree with _at most_ as many leaves as the size indicates.

Do not forget to implement `shrink`!

### Subtask 3.3.3

Write a `QuickCheck` property
```haskell
propFunctor :: ((Int -> Int) -> Tree Int -> Tree Int)
            -> Tree Int
            -> Property
```
that takes an "`fmap`-like" function and checks the first functor law
for it.
Make sure `quickCheck $ propFunctor fmap` passes all tests!

### Subtasj 3.3.4

Implement a non-looping and non-crashing
function
```haskell
fmap' :: (a -> b) -> Tree a -> Tree b
```
that _is not_ `fmap`.

Use `QuickCheck` and 3.3.3
to prove that `fmap'` does not obey the first
functor law, and give the minimal counter example
that is found. It should be a tree with one node and two leaves.
If `QuickCheck` falsifies the property, but fails to report
such a nice and small counter example, you should reconsider
your implementation of `shrink`!

## W3.4 Number of distinguishable values of datatypes

In the presence of laziness, datatypes contain more distinguishable
values than one might expect.

Intuitively, values `a1` and `a2` of type `A` are distinguishable if
there is a context in which `a1` and `a2` behave differently. For the
purposes, of this exercise, we are going to consider functions
```haskell
f :: A -> Int
```
and say `a1` and `a2` are distinguishable if `f a1` and `f a2` have
different results. Note furthermore that for the purposes of this
exercise, we treat all forms of nontermination and crashing as equivalent.

How many dinstinguishable Haskell values are there of the following types:

- `Bool`
- `Either Bool Bool`
- `(Bool, Bool)`
- `Maybe Bool`
- `Bool -> Bool`

For `(Bool, Bool)` only, write functions that distinguish all of the
different values.
