module Robot
    ( Robot (..), RobotSem (..), Command (..)
    , square
    , foldRobot
    , energy, energySem, txt, txtSem, simplifySem
    , remoteSem
    ) where

data Robot =
      Rest                 -- 0 energy; doesn't do anything, doesn't take any time
    | Go Int               -- 1 energy unit per step; go backwards if step number is negative
    | TurnRight            -- 1 energy unit
    | TurnLeft             -- 1 energy unit
    | Seq Robot Robot
    | Repeat Int Robot     -- just rest of number of repetitions is zero or negative
    deriving (Show, Read)

square :: Int -> Robot
square n = Repeat 4 (Seq (Go n) TurnRight)

energy :: Robot -> Int
energy Rest         = 0
energy (Go n)       = abs n
energy TurnRight    = 1
energy TurnLeft     = 1
energy (Seq r1 r2)  = energy r1 + energy r2
energy (Repeat n r) = (max 0 n) * energy r

energySem :: RobotSem Int
energySem = RobotSem
    { rest      = 0
    , go        = abs
    , turnRight = 1
    , turnLeft  = 1
    , seq_      = (+)
    , repeat_   = \n x -> (max 0 n) * x
    }

txt :: Robot -> String
txt Rest            = "Rest"
txt (Go n)          = "(Go " ++ show n ++ ")"
txt TurnRight       = "->"
txt TurnLeft        = "<-"
txt (Seq r1 r2)     = "(" ++ txt r1 ++ " " ++ txt r2 ++ ")"
txt (Repeat n r)    = "(Repeat " ++ show n ++ " " ++ txt r ++ ")"

txtSem :: RobotSem String
txtSem = RobotSem
    { rest      = "Rest"
    , go        = \n -> "(Go " ++ show n ++ ")"
    , turnRight = "->"
    , turnLeft  = "<-"
    , seq_      = \t1 t2 -> "(" ++ t1 ++ " " ++ t2 ++ ")"
    , repeat_   = \n t -> "(Repeat " ++ show n ++ " " ++ t ++ ")"
    }

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

simplifySem :: RobotSem Robot
simplifySem = RobotSem
    { rest      = Rest
    , go        = \n -> if n == 0 then Rest else Go n
    , turnRight = TurnRight
    , turnLeft  = TurnLeft
    , seq_      = \s1 s2 -> case (s1, s2) of
                                (Rest, _)             -> s2
                                (_, Rest)             -> s1
                                (Go m, Go n)          -> let k = m + n in if k == 0 then Rest else Go k
                                (TurnRight, TurnLeft) -> Rest
                                (TurnLeft, TurnRight) -> Rest
                                (_, _)                -> Seq s1 s2
    , repeat_   = \n s -> if n <= 0 then Rest
                                    else if n == 1 then s
                                                   else Repeat n s
    }

data Command = F | L | R
    deriving (Show, Read, Eq, Ord)

remoteSem :: RobotSem [Command]
remoteSem = RobotSem
    { rest      = []
    , go        = \n -> if n >= 0 then replicate n F
                                  else [L, L] ++ replicate (-n) F ++ [L, L]
    , turnRight = [R]
    , turnLeft  = [L]
    , seq_      = (++)
    , repeat_   = \n cs -> if n <= 0 then []
                                     else concat $ replicate n cs
    }


-- running around the track once costs me 1000 calories
-- How much does running around the track 7 times cost me?
