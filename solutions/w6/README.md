# Weekly Assignments 6

### To be submitted: Friday, 11 September 2020, 12:30 MNG

Note that some tasks may deliberately ask you to look at concepts or libraries
that we have not yet discussed in detail. But if you are in doubt about the
scope of a task, by all means ask.

Please try to write high-quality code at all times!
This means in particular that you should add comments to all parts
that are not immediately obvious. Please also pay attention to
stylistic issues. The goal is always to submit code that does not
just correctly do what was asked for, but also could be committed
without further changes to an imaginary company codebase.

## W6.1 The Robot DSL

Recall the `Robot`-DSL from our example session:

```
data Robot =
      Rest
    | Go Int
    | TurnRight
    | TurnLeft
    | Seq Robot Robot
    | Repeat Int Robot

data RobotSem d = RobotSem
    { rest      :: d
    , go        :: Int -> d
    , turnRight :: d
    , turnLeft  :: d
    , seq_      :: d -> d -> d
    , repeat_   :: Int -> d -> d
    }

foldRobot :: RobotSem d -> Robot -> d
foldRobot sem Rest         = rest sem
foldRobot sem (Go n)       = go sem n
foldRobot sem TurnRight    = turnRight sem
foldRobot sem TurnLeft     = turnLeft sem
foldRobot sem (Seq r1 r2)  = seq_ sem (foldRobot sem r1) (foldRobot sem r2)
foldRobot sem (Repeat n r) = repeat_ sem n (foldRobot sem r)
```

### Subtask W6.1.1

Define semantics `stepsSem :: RobotSem Int` for calculating the total number of steps
(forwards and backwards) that the robot will go (turning does not count, only going).

### Subtask W6.1.2

Implement a function `squareDistance :: Robot -> Natural`
thaz calculates the _square_ of the distance (measured in steps)
from the starting point to the end point
for a given command in the `Robot`-DSL,

### Subtask W6.1.3

Consider the following simplified DSL for the same robot.
```
data SimpleRobot =
      SimpleRest
    | SimpleGo Int SimpleRobot
    | SimpleTurnRight SimpleRobot
    | SimpleTurnLeft SimpleRobot
```

Define semantics `simpleSem :: RobotSem SimpleRobot` which faithfully translate
a command from the `Robot`-DSL to the `SimpleRobot`-DSL.

### Subtask W6.1.4

Define the type `SimpleRobotSem d` which describes semantics of type `d`
for the `SimpleRobot`-DSL. Also define the corresponding catamorphism
`foldSimpleRobot :: SimpleRobotSem d -> SimpleRobot -> d` and
give semantics `simpleEnergySem :: SimpleRobotSem Int` for the calculation
of energy consumption.

### Subtask W6.1.5

Make `Robot` an instance of the `Arbitrary`-class and then write a
property `prop_energy_simpleEnergy :: Robot -> Property`
which checks that energy consumption for a `Robot`-command does not change
when we first translate to a `SimpleRobot`-command and calculate energy consumption
afterwards.

## W6.2 Finite Sets of Integers

Consider the following DSL for describing _finite sets of integers_:

```
data FinSet =
      Interval Integer Integer
    | Union FinSet FinSet
    | Intersection FinSet FinSet
```

### W6.2.1

Define type `FinSetSem d` to describe semantics of type `d` for the `FinSet`-DSL
and implement the corresponding catamorphism
`foldFinSet :: FinSetSem d -> FinSet -> d`.

### W6.2.2

Implement semantics `elemSem :: FinSetSem (Integer -> Bool)`
for `FinSet` which allow you to decide whether a given integer
is an element of the given set.

### W6.2.3

Implement semantics `minMaxSem :: FinSetSem (Maybe (Integer, Integer))`
that allow you to compute minimum and maximum of a `FinSet`
(or `Nothing` if the set is empty).

### W6.2.4

Define semantics `intSetSem :: FinSetSem IntSet` to convert a `FinSet` into the
`IntSet` (from `Data.IntSet`) representing the same set of integers.

## W6.3 Propositions

Recall the DSL for _propositions_ and related definitions from the lecture:

```
data Prop =
      Var String
    | T
    | F
    | Not Prop
    | And Prop Prop
    | Or Prop Prop
    deriving (Show, Eq)

data PropSem d = PropSem
    { var_ :: String -> d
    , t_   :: d
    , f_   :: d
    , not_ :: d -> d
    , and_ :: d -> d -> d
    , or_  :: d -> d -> d
    }

foldProp :: PropSem d -> Prop -> d
foldProp PropSem{ var_, t_, f_, not_, and_, or_ } = go
  where
    go (Var s)   = var_ s
    go T         = t_
    go F         = f_
    go (Not p)   = not_ $ go p
    go (And p q) = and_ (go p) (go q)
    go (Or p q)  = or_ (go p) (go q)

type Env = String -> Bool

empty :: Env
empty = const False

extend :: String -> Bool -> Env -> Env
extend s b env = \s' -> if s' == s then b else env s'

evalSem :: PropSem (Env -> Bool)
evalSem = PropSem
    { var_ = flip ($)
    , t_   = const True
    , f_   = const False
    , not_ = (not .)
    , and_ = \p q env -> p env && q env
    , or_  = \p q env -> p env || q env
    }

varsSem :: PropSem (Set String)
varsSem = PropSem
    { var_ = S.singleton
    , t_   = S.empty
    , f_   = S.empty
    , not_ = id
    , and_ = S.union
    , or_  = S.union
    }
```

### Subtask W6.3.1

Implement a function `sat :: Prop -> Maybe Env` which checks
whether the given `Prop` is _satisfiable_,
i.e. whether there exists an environment in which the proposition evaluates to `True`.
If yes, `Just` such an environment is returned.
If no, `Nothing` is returned.

### Subtask W6.3.2

It is a well-known fact that instead of using `not`, `(&&)` and `(||)`
to construct propositions, just using the single function

```
nand :: Bool -> Bool -> Bool`
nand x y = not $ x && y
```

is sufficient.

Consider the following DSL for expressing propositions using the `nand`-operation:

```
data Nand = Var' String | Nand Nand Nand
```

Define type `NandSem d` for semantics of the `Nand`-DSL
and the associated catamorphism `foldNand :: NandSem d -> Nand -> d`.

Define semantics `evalSem' :: NandSem (Env -> Bool)` to evaluate
`Nand`-expressions in an environment.

Finally, define semantics `nandSem :: PropSem Nand` to convert
from the `Prop`-DSL to equivalent propositions in the `Nand`-DSL.


