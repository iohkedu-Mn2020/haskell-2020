module Chat
    ( PortNumber
    , run
    ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Set (Set)
import qualified Data.Set as S
import           Network
import           System.IO

type Name = String

data Message =
      Joined Name
    | Disconnected Name
    | Broadcast Name String
    | Tell Name Name String
    | Kick Name Name
    deriving (Show, Read, Eq, Ord)

run :: PortNumber -> IO ()
run port = do
    names <- newTVarIO S.empty
    ch    <- newBroadcastTChanIO
    putStrLn $ "starting server at port " ++ show port
    s <- listenOn $ PortNumber port
    forever $ do
        (h, hn, p) <- accept s
        name       <- newTVarIO Nothing
        putStrLn $ "accepted connection from host " ++ show hn ++ ", port " ++ show p
        void $ forkFinally (handleClient h name names ch) $ const $ do
            hClose h
            m <- readTVarIO name
            case m of
                Nothing -> return ()
                Just n  -> atomically $ do
                    modifyTVar' names $ S.delete n
                    writeTChan ch $ Disconnected n

handleClient :: Handle
             -> TVar (Maybe Name)
             -> TVar (Set Name)
             -> TChan Message
             -> IO ()
handleClient h name names ch = do
    hSetNewlineMode h universalNewlineMode
    hSetBuffering h LineBuffering
    n   <- negotiateName h name names
    ch' <- atomically $ dupTChan ch
    let sendMessage = atomically . writeTChan ch
        readMessage = atomically $ readTChan ch'
    sendMessage $ Joined n
    incoming h n sendMessage `race_` outgoing h n readMessage

negotiateName :: Handle -> TVar (Maybe Name) -> TVar (Set Name) -> IO Name
negotiateName h name names = go
  where
    go = do
        hPutStrLn h "What is your name?"
        n <- hGetLine h
        if null n
            then hPutStrLn h "The name must not be empty!" >> go
            else do
                b <- atomically $ do
                    ns <- readTVar names
                    if S.member n ns
                        then return False
                        else do
                            let ns' = S.insert n ns
                            writeTVar names ns'
                            writeTVar name $ Just n
                            return True
                if b then return n
                     else do
                        hPutStrLn h "That name is already in use, please choose another one!"
                        go

incoming :: Handle -> Name -> (Message -> IO ()) -> IO ()
incoming h n sendMessage = go
  where
    go :: IO ()
    go = do
        l <- hGetLine h
        if l == "/quit"
            then return ()
            else do
                case words l of
                    "/tell" : whom : msg -> sendMessage $ Tell n whom $ unwords msg
                    ["/kick", whom] -> sendMessage $ Kick n whom
                    _ -> sendMessage $ Broadcast n l
                go

outgoing :: Handle -> Name -> IO Message -> IO ()
outgoing h n readMessage = go
  where
    go :: IO ()
    go = do
        msg <- readMessage
        case msg of
            Joined who
                | who /= n -> hPutStrLn h ("*** " ++ who ++ " joined") >> go
            Disconnected who -> hPutStrLn h ("*** " ++ who ++ " disconnected") >> go
            Broadcast from txt
                | from /= n -> hPutStrLn h (from ++ ": " ++ txt) >> go
            Tell from to txt
                | to == n -> hPutStrLn h (">>> " ++ from ++ ": " ++ txt) >> go
            Kick who whom
                | whom == n -> hPutStrLn h $ "### kicked by " ++ who
            _ -> go
