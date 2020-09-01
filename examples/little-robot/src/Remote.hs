module Remote
    ( PortNumber
    , server
    ) where

import Control.Monad       (forever)
import Network
import Robot               (Command (..))
import System.Console.ANSI
import System.IO
import System.IO.Error     (catchIOError)
import Text.Read

data Orientation = North | East | South | West
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

data RobotState = RobotState
    { rsRow :: Int
    , rsCol :: Int
    , rsOr  :: Orientation
    } deriving (Show, Read, Eq, Ord)

update :: Command -> RobotState -> RobotState
update F s = case rsOr s of
    North -> s{rsRow = rsRow s - 1}
    East  -> s{rsCol = rsCol s + 1}
    South -> s{rsRow = rsRow s + 1}
    West  -> s{rsCol = rsCol s - 1}
update L s = case rsOr s of
    North -> s{rsOr = West}
    East  -> s{rsOr = North}
    South -> s{rsOr = East}
    West  -> s{rsOr = South}
update R s = case rsOr s of
    North -> s{rsOr = East}
    East  -> s{rsOr = South}
    South -> s{rsOr = West}
    West  -> s{rsOr = North}

symbol :: Orientation -> Char
symbol North = '^'
symbol East  = '>'
symbol South = 'v'
symbol West  = '<'

initialState :: IO RobotState
initialState = do
    Just (height, width) <- getTerminalSize
    return RobotState
        { rsRow = height `div` 2
        , rsCol = width `div` 2
        , rsOr  = North
        }

server :: PortNumber -> IO ()
server port = do
    hSetBuffering stdout NoBuffering
    hideCursor
    s <- listenOn $ PortNumber port
    forever $ do
        h <- getHandle s
        catchIOError (withHandle h) $ const (hClose h)
  where
    getHandle :: Socket -> IO Handle
    getHandle s = do
        clearScreen
        setCursorPosition 0 0
        putStrLn $ "waiting for connection on port " ++ show port ++ " ..."
        (h, _, _) <- accept s
        return h

    withHandle :: Handle -> IO ()
    withHandle h = do
        hSetBuffering h LineBuffering
        clearScreen
        s <- initialState
        go s
      where
        go s = do
            setCursorPosition 0 0
            print s
            setCursorPosition (rsRow s) (rsCol s)
            putChar $ symbol $ rsOr s
            c <- getCommand
            setCursorPosition (rsRow s) (rsCol s)
            putChar ' '
            go $ update c s

        getCommand :: IO Command
        getCommand = do
            mc <- readMaybe <$> hGetLine h
            case mc of
                Nothing -> getCommand
                Just c  -> return c
