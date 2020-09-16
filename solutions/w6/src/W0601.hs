{-# LANGUAGE NamedFieldPuns #-}

module W0601
    ( -- * the Robot DSL
      Robot (..), RobotSem (..), foldRobot, energySem
      -- * Subtask W6.1.1
    , stepsSem
      -- * Subtask W6.1.2
    , squareDistance
      -- * Subtask W6.1.3
    , SimpleRobot (..), simpleSem
      -- * Subtask W6.1.4
    , SimpleRobotSem (..), foldSimpleRobot, simpleEnergySem
      -- * Subtask W6.1.5
    , prop_energy_simpleEnergy
    ) where

import Control.Monad.State
import Numeric.Natural     (Natural)
import Test.QuickCheck

-- | DSL for a simple robot.
data Robot =
      Rest                 -- ^ Does not consume energy; does not do anything, does not take any time.
    | Go Int               -- ^ Goes the given number of steps forwards, taking one energy unit per step. Goes backwards if the number of steps is negative.
    | TurnRight            -- ^ Turns 90 degrees to the right, which costs one energy unit.
    | TurnLeft             -- ^ Turns 90 degrees to the left, which costs one energy unit.
    | Seq Robot Robot      -- ^ Sequences the two given commands.
    | Repeat Int Robot     -- ^ Repeats the given command as often as indicated by the first argument. Just rests if the number of repetitions is zero or negative.
    deriving Show

-- | Data needed to define semantics of type @d@ for the 'Robot'-DSL.
data RobotSem d = RobotSem
    { rest      :: d
    , go        :: Int -> d
    , turnRight :: d
    , turnLeft  :: d
    , seq_      :: d -> d -> d
    , repeat_   :: Int -> d -> d
    }

-- | Calculates the semantics specified by the first argument.
foldRobot :: RobotSem d -> Robot -> d
foldRobot sem Rest         = rest sem
foldRobot sem (Go n)       = go sem n
foldRobot sem TurnRight    = turnRight sem
foldRobot sem TurnLeft     = turnLeft sem
foldRobot sem (Seq r1 r2)  = seq_ sem (foldRobot sem r1) (foldRobot sem r2)
foldRobot sem (Repeat n r) = repeat_ sem n (foldRobot sem r)

-- | Defines semantics of energy consumption for the 'Robot'-DSL.
--
-- >>> foldRobot energySem $ Repeat 4 $ Go 5 `Seq` TurnRight
-- 24
--
energySem :: RobotSem Int
energySem = RobotSem
    { rest      = 0
    , go        = abs
    , turnRight = 1
    , turnLeft  = 1
    , seq_      = (+)
    , repeat_   = \n x -> (max 0 n) * x
    }

-- Subtask W6.1.1

-- | Semantics for calculating the total number of steps (forwards and backwards) that
-- the robot will go (turning does not count, only going).
--
-- >>> foldRobot stepsSem $ Go 222 `Seq `Go (-333) `Seq` Repeat 1000 TurnRight
-- 555
--
stepsSem :: RobotSem Int
stepsSem = RobotSem
    { rest      = 0
    , go        = abs
    , turnRight = 0
    , turnLeft  = 0
    , seq_      = (+)
    , repeat_   = \n x -> max 0 n * x
    }

-- Subtask W6.1.2

-- | Calculates the /square/ of the distance (measured in steps)
-- from the starting point to the end point for the given command,
--
-- >>> squareDistance $ Go 1000 `Seq` Go (-1000)
-- 0
--
-- >>> squareDistance $ Go 3 `Seq` Repeat 101 TurnLeft `Seq` Go 4
-- 25
--
-- >>> squareDistance $ Repeat 444 $ Go 101 `Seq` TurnRight
-- 0
--
squareDistance :: Robot -> Natural
squareDistance r =
    let S x y _ = foldRobot sSem r $ S 0 0 North
        x'      = fromIntegral $ abs x
        y'      = fromIntegral $ abs y
    in  x' * x' + y' * y'

data Orientation = North | East | South | West deriving Show

data S = S Int Int Orientation

sSem :: RobotSem (S -> S)
sSem = RobotSem
    { rest      = id
    , go        = \d (S x y o) -> case o of
                                    North -> S x (y + d) o
                                    East  -> S (x + d) y o
                                    South -> S x (y - d) o
                                    West  -> S (x - d) y o
    , turnRight = \(S x y o) -> case o of
                                    North -> S x y East
                                    East  -> S x y South
                                    South -> S x y West
                                    West  -> S x y North
    , turnLeft  = \(S x y o) -> case o of
                                    North -> S x y West
                                    East  -> S x y North
                                    South -> S x y East
                                    West  -> S x y South
    , seq_      = \f g -> g . f
    , repeat_   = \n -> execState . replicateM_ n . modify
    }

-- Subtask W6.1.3

-- | A simpler DSL for the same robot.
data SimpleRobot =
      SimpleRest                  -- ^ Does not consume energy; does not do anything, does not take any time.
    | SimpleGo Int SimpleRobot    -- ^ Goes the given number of steps forwards (or backwards if the number is negative), then continues with the second argument.
    | SimpleTurnRight SimpleRobot -- ^ Turns 90 degrees to the right, then continues with the second argument.
    | SimpleTurnLeft SimpleRobot  -- ^ Turns 90 degrees to the left, then continues with the second argument.
    deriving Show

-- | Semantics for faithfully translating the 'Robot'-DSL into the simplified 'SimpleRobot'-DSL.
--
-- >>> foldRobot simpleSem $ (Go 1 `Seq` Go 2) `Seq` Go 3
-- SimpleGo 1 (SimpleGo 2 (SimpleGo 3 SimpleRest))
--
-- >>> foldRobot simpleSem $ Repeat 4 $ Go 2 `Seq` TurnLeft
-- SimpleGo 2 (SimpleTurnLeft (SimpleGo 2 (SimpleTurnLeft (SimpleGo 2 (SimpleTurnLeft (SimpleGo 2 (SimpleTurnLeft SimpleRest)))))))
--
simpleSem :: RobotSem SimpleRobot
simpleSem = RobotSem
    { rest = SimpleRest
    , go   = \n -> SimpleGo n SimpleRest
    , turnRight = SimpleTurnRight SimpleRest
    , turnLeft  = SimpleTurnLeft SimpleRest
    , seq_      = seq'
    , repeat_   = repeat'
    }
  where
    seq' :: SimpleRobot -> SimpleRobot -> SimpleRobot
    seq' SimpleRest          s = s
    seq' (SimpleGo n r)      s = SimpleGo n      $ seq' r s
    seq' (SimpleTurnRight r) s = SimpleTurnRight $ seq' r s
    seq' (SimpleTurnLeft r)  s = SimpleTurnLeft  $ seq' r s

    repeat' :: Int -> SimpleRobot -> SimpleRobot
    repeat' n r
        | n <= 0    = SimpleRest
        | otherwise = seq' r $ repeat' (n - 1) r

-- Subtask W6.1.4

-- Data needed to define semantics of type @d@ for the 'SimpleRobot'-DSL.
data SimpleRobotSem d = SimpleRobotSem
    { rest'  :: d
    , go'    :: Int -> d -> d
    , right' :: d -> d
    , left'  :: d -> d
    }

-- | Calculates the semantics specified by the first argument.
foldSimpleRobot :: SimpleRobotSem d -> SimpleRobot -> d
foldSimpleRobot SimpleRobotSem { rest', go', right', left' } = go
  where
    go SimpleRest             = rest'
    go (SimpleGo n cont)      = go' n $ go cont
    go (SimpleTurnRight cont) = right' $ go cont
    go (SimpleTurnLeft cont)  = left' $ go cont

-- | Defines semantics of energy consumption for the 'SimpleRobot'-DSL.
--
-- >>> foldSimpleRobot simpleEnergySem $ SimpleGo 5 $ SimpleTurnRight $ SimpleGo (-3) SimpleRest
-- 9
--
simpleEnergySem :: SimpleRobotSem Int
simpleEnergySem = SimpleRobotSem
    { rest'  = 0
    , go'    = \n d -> abs n + d
    , right' = succ
    , left'  = succ
    }

-- Subtask W6.1.5

instance Arbitrary Robot where

    arbitrary = oneof
        [ pure Rest
        , Go <$> arbitrary
        , pure TurnRight
        , pure TurnLeft
        , Seq <$> arbitrary <*> arbitrary
        , Repeat <$> arbitrary <*> arbitrary
        ]

    shrink Rest         = []
    shrink (Go n)       = Rest : (Go <$> shrink n)
    shrink TurnRight    = [Rest]
    shrink TurnLeft     = [Rest]
    shrink (Seq x y)    = x : y : [Seq x' y | x' <- shrink x] ++ [Seq x y' | y' <- shrink y]
    shrink (Repeat n x) = x : [Repeat n' x | n' <- shrink n] ++ [Repeat n x' | x' <- shrink x]

-- | States that energy consumption of a command in the 'Robot'-DSL as specified by 'energySem'
-- is the same as energy consumption of the translated command in the 'SimpleRobot'-DSL as specified by 'simpleEnergySem'.
--
-- prop> prop_energy_simpleEnergy
--
prop_energy_simpleEnergy :: Robot -> Property
prop_energy_simpleEnergy r = foldSimpleRobot simpleEnergySem (foldRobot simpleSem r) === foldRobot energySem r
