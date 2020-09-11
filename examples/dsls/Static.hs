module Static where

import Marlowe

--             smart contract language
--
--  Turing complete        | not Turing complete
--  ------------------------------------------------------
--                         |
--  more powerful          | suitable for given
--  can express arbitrary  | domain
--  algorithms             |
--                         |
--                         |
--  Halting Problem        | more powerful static analysis
--                         |
--                         |
--  use cost budget to     | do nothing (Bitcoin Script)
--  ensure run time limit  |
--  (Ethereum, Plutus)     | Marlowe - designed to allow
--                         | static analysis

clup :: Contract -> Integer
clup Close           = 0
clup (Pay _ _ _ _ c) = clup c
clup (If _ t e)      = max (clup t) (clup e)
clup (Let _ _ c)     = clup c
clup (Assert _ c)    = clup c
clup (When cs t c)   = maximum $ (getSlot t) : clup c : map clup' cs
  where
    clup' :: Case -> Integer
    clup' (Case _ c') = clup c'

ex1 :: Contract
ex1 = When [] (Slot 1000) Close

-- maximum $ getSlot t : clup c : map clup' cs
--                                          []
-- maximum $ [1000, clup Close]
-- maximum $ [1000, 0]
-- 1000



