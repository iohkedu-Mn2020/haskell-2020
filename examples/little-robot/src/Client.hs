module Client
    ( client
    ) where

import Control.Concurrent
import Control.Monad
import Network
import Robot
import System.IO
import Text.Read


client :: PortNumber -> IO ()
client port = do
    hSetBuffering stdout NoBuffering
    putStr $ "trying to connect to port " ++ show port ++ " ... "
    h <- connectTo "127.0.0.1" $ PortNumber port
    putStrLn $ "connected"
    forever $ do
        putStr "> "
        mr <- readMaybe <$> getLine
        case mr of
            Nothing -> putStrLn "illegal instruction"
            Just r  -> do
                let cs = foldRobot remoteSem r
                putStrLn $ "sending commands: " ++ show cs
                mapM_ (\c -> hPrint h c >> threadDelay 150000) cs
