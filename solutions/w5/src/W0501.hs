{-# LANGUAGE RankNTypes #-}

module W0501
    ( -- * Subtask W5.1.1
      _Natural
      -- * Subtask W5.1.2
    , _TheOne
      -- * Subtask W5.1.3
    , Checked (..), _Check
    ) where

import Numeric.Natural
import Optics

-- Subtask W5.1.1

-- | Focuses on the natural number "inside" an integer.
--
-- >>> preview _Natural 42
-- Just 42
-- >>> preview _Natural (-7)
-- Nothing
-- >>> review _Natural 8
-- 8
--
_Natural :: Prism' Integer Natural
_Natural = prism' pr fromIntegral
  where
    pr n
        | n >= 0    = Just $ fromIntegral n
        | otherwise = Nothing

-- Subtask W5.1.2

-- | Focuses on the given element.
--
-- >>> preview (_TheOne 'x') 'x'
-- Just ()
-- >>> preview (_TheOne 'x') 'y'
-- Nothing
-- >>> review (_TheOne 'x') ()
--'x'
--
_TheOne :: Eq a => a -> Prism' a ()
_TheOne a = prism' pr (const a)
  where
    pr b
        | b == a    = Just ()
        | otherwise = Nothing

-- Subtask W5.1.3

newtype Checked a = Checked {unChecked :: a} deriving Show

-- | Focuses on an element satisfying the given property.
--
-- >>> preview (_Check odd) 7
-- Just (Checked {unChecked = 7})
-- >>> preview (_Check odd) 42
-- Nothing
-- >>> review (_Check odd) (Checked 3)
-- 3
--
_Check :: (a -> Bool) -> Prism' a (Checked a)
_Check p = prism' pr unChecked
  where
    pr a
        | p a       = Just $ Checked a
        | otherwise = Nothing
