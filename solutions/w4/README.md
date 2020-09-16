
# Weekly Assignments 4

### To be submitted: Friday, 28 August 2020, 12:30

Note that some tasks may deliberately ask you to look at concepts or libraries
that we have not yet discussed in detail. But if you are in doubt about the
scope of a task, by all means ask.

Please try to write high-quality code at all times!
This means in particular that you should add comments to all parts
that are not immediately obvious. Please also pay attention to
stylistic issues. The goal is always to submit code that does not
just correctly do what was asked for, but also could be committed
without further changes to an imaginary company codebase.

## W4.1 Nested datatypes

Recall the type of perfect trees,
```haskell
data Perfect a = Z a | S (Perfect (a, a))
```

### Subtask 4.1.1

Define a function
```haskell
reversePerfect :: Perfect a -> Perfect a
```
that creates a perfect tree of the same shape, but with
all the leaves in reversed order.

### Subtask 4.1.2

Define a function
```haskell
index :: Perfect a -> Int -> Maybe a
```
that looks up the element with the given index (from the left,
starting at `0`) and fails if the index is out of bounds.
The function should have logarithmic complexity in the number
of elements in the tree.

### Subtask 4.1.3

Define a function
```haskell
build :: Int -> [a] -> Perfect a
```
such that `build n xs` builds a tree of height `n` (i.e., with
`2 ^ n` elements), taking the leaves from the list (from left
to right). The function is allowed to crash if the list is too
short. Use the `State` monad.

Note: If this is too difficult, at least try to define the
much simpler function
```haskell
build :: Int -> Perfect ()
```
that builds a perfect tree of the given height with unit values
in the leaves.

## W4.2 Benchmarking

Have a look at the [`criterion`](https://hackage.haskell.org/package/criterion)
package for benchmarking.
Using criterion, test the efficiency of indexing the first,
the middle and the last element of a perfect tree of different
height. See how the runtime evolves with growing trees.

Try to pay attention to the following:

- the time for building the tree should not be measured,
- the time for indexing should not be cached due to laziness.

Turn the benchmark into a benchmark suite in the Cabal file
so that it can be run via `cabal bench`.
Try to generate a nice html-report as well.

## W4.3 Mini private keys

The Bitcoin Wiki at

  https://en.bitcoin.it/wiki/Mini_private_key_format

describes a format for mini private keys.

You are allowed to use the
[`cryptonite`](https://hackage.haskell.org/package/cryptonite),
[`base16-bytestring`](https://hackage.haskell.org/package/base16-bytestring) and
[`base58-bytestring`](https://hackage.haskell.org/package/base58-bytestring) packages.

### Subtask 4.3.1

Define a validator for mini private keys that determines
if a given input is a valid mini private key and outputs
the corresponding full private key.

This validator should be available as a library function
and as an executable that reads the
mini private key to test from the standard input.
The executable should output the private key to standard output
if the mini key is valid and `INVALID` otherwise.

### Subtask 4.3.2

Define a function that transforms a private key into
Wallet Import Format as described on

  https://en.bitcoin.it/wiki/Wallet_import_format

This functionality
should be available both as a library function and as an
exectuable.
The executable should read a private key from standard input
and output the Wallet Import Format to standard output
(or `INVALID` if the key is invalid).
You can assume mainnet and uncompressed public keys,
but you can also have a look at
[optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
for proper command-line parsing in Haskell and then
support testnet and compressed public keys as well.

### Subtask 4.3.3

Define a function that transforms Wallet Import Format into a private key as described on

  https://en.bitcoin.it/wiki/Wallet_import_format

This functionality
should be available both as a library function and as an
exectuable.
The executable should read Wallet Import Format from standard input
and output the private key to standard output
(or `INVALID` if the Wallet Import Format is invalid).
You can assume mainnet and uncompressed public keys,
but you can also have a look at
[optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
for proper command-line parsing in Haskell and then
support testnet and compressed public keys as well.
