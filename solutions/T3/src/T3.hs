{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : T3
Description : third test for the IOHK Haskell Course in Ulaanbaartar 2020
Copyright   : (c) Lars Brünjes, 2019
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains the third test for the IOHK Haskell Course in Ulaanbaartar 2020.
Many doctests have been provided for your convenience. You can run them with
@cabal test@ to monitor your progress.

__Please note: From now on, we won't accept any submissions that do not compile!__
-}
module T3 where

import Control.Applicative
import Parser
import Test.QuickCheck

data Weird a = ZeroOne (Maybe a) | TwoMore a a [a]
    deriving (Show, Read, Eq)

-- | Write a 'Functor' instance for 'Weird'.
-- Do __/not/__ use the @DeriveFunctor@-extension!
--
-- >>> fmap (+ 1) $ ZeroOne (Just 3)
-- ZeroOne (Just 4)
--
-- >>> fmap (* 2) $ TwoMore 1 2 [3, 4]
-- TwoMore 2 4 [6,8]
--
instance Functor Weird where

    fmap :: (a -> b) -> Weird a -> Weird b
    fmap f (ZeroOne m)      = ZeroOne $ f <$> m
    fmap f (TwoMore x y xs) = TwoMore (f x) (f y) $ f <$> xs

-- | Write a 'Foldable' instance for 'Weird'.
-- Do __/not/__ use the @DeriveFoldable@-extension!
--
-- >>> sum $ ZeroOne Nothing
-- 0
--
-- >>> sum $ ZeroOne (Just 42)
-- 42
--
-- >>> sum $ TwoMore 1 2 [3, 4]
-- 10
--
instance Foldable Weird where

    foldr :: (a -> b -> b) -> b -> Weird a -> b
    foldr f b (ZeroOne m)      = foldr f b m
    foldr f b (TwoMore x y xs) = f x $ f y $ foldr f b xs

-- | Write a 'Traversable' instance for 'Weird'.
-- Do __/not/__ use the @DeriveTraversable@-extension!
--
-- >>> traverse print $ ZeroOne (Nothing :: Maybe String)
-- ZeroOne Nothing
--
-- >>> traverse print $ ZeroOne (Just True)
-- True
-- ZeroOne (Just ())
--
-- >>> traverse print $ TwoMore False True [True, False]
-- False
-- True
-- True
-- False
-- TwoMore () () [(),()]
--
instance Traversable Weird where

    traverse :: Applicative f => (a -> f b) -> Weird a -> f (Weird b)
    traverse f (ZeroOne m)      = ZeroOne <$> traverse f m
    traverse f (TwoMore x y xs) = TwoMore <$> f x <*> f y <*> traverse f xs

-- | Write a 'Parser' that consumes one arbitrary character
-- and returns it as the result.
--
-- >>> runParser char "xyz"
-- [('x',"yz")]
--
-- >>> runParser char ""
-- []
--
-- >>> runParser char "&"
-- [('&',"")]
--
char :: Parser Char Char
char = satisfy $ const True

-- | Converts a @'Weird' 'Char'@ to a 'String'.
--
-- >>> showWeird $ ZeroOne $ Just 'x'
-- "{-x-}"
--
-- >>> showWeird $ ZeroOne Nothing
-- "{--}"
--
-- >>> showWeird $ TwoMore 'a' 'b' "cde"
-- "{-abcde-}"
--
showWeird :: Weird Char -> String
showWeird w = "{-" ++ f w ++ "-}"
  where
    f (ZeroOne Nothing)  = ""
    f (ZeroOne (Just c)) = [c]
    f (TwoMore c d xs)   = c : d : xs

-- | Write a 'Parser' that can parse output from 'showWeird' back to type
-- @'Weird' 'Char'@.
--
-- >>> runParser (parseWeird <* eof) "{-abc-}"
-- [(TwoMore 'a' 'b' "c","")]
--
-- >>> runParser (parseWeird <* eof) "{--}"
-- [(ZeroOne Nothing,"")]
--
-- >>> runParser (parseWeird <* eof) "{-%-}"
-- [(ZeroOne (Just '%'),"")]
--
-- >>> runParser (parseWeird <* eof) "{-noend"
-- []
--
parseWeird :: Parser Char (Weird Char)
parseWeird = token '{' *> token '-' *> p <* token '-' <* token '}'
  where
    p :: Parser Char (Weird Char)
    p =     pure (ZeroOne Nothing)
        <|> ZeroOne . Just <$> char
        <|> TwoMore        <$> char <*> char <*> many char

-- | Write an 'Arbitrary' instance for @'Weird' a@. Don't worry too much about
-- 'shrink'.
instance Arbitrary a => Arbitrary (Weird a) where

    arbitrary :: Gen (Weird a)
    arbitrary = oneof [ZeroOne <$> arbitrary, TwoMore <$> arbitrary <*> arbitrary <*> arbitrary]

-- | Write a QuickCheck property that checks that starting with a
-- @w :: 'Weird' 'Char'@, converting it to a 'String' with 'showWeird',
-- then parsing it back with 'parseWeird' will give the original
-- @w@.
-- Alternatively, if you failed to implement 'parseWeird',
-- check the roundtrip property for 'show' and 'read': Starting with
-- @w :: 'Weird' 'Char'@, converting it to a 'String' with 'show',
-- then parsing it back with 'read' will give the original
-- @w@.
--
-- >>> quickCheck prop_roundtrip
-- +++ OK, passed 100 tests.
--
prop_roundtrip :: Weird Char -> Property
prop_roundtrip w = runParser (parseWeird <* eof) (showWeird w) === [(w, "")]
