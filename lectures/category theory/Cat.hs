module Cat where

-- f     :: S ------ f ------> T
--
-- g     ::                    T ------- g --------> U
--
-- g . f :: S --------------- g . f ---------------> U
--
--
-- id    :: S ---- id S ----> S
--
-- laws ::
--
--  - associativity:  f . (g . h) = (f . g) . h
--  - left-identity:  id . f = f
--  - right-identity: f . id = f
